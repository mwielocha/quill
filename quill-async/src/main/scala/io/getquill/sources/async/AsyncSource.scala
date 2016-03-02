package io.getquill.sources.async

import scala.annotation.implicitNotFound
import scala.concurrent.Await
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.duration.Duration
import scala.util.Try
import com.github.mauricio.async.db.Configuration
import com.github.mauricio.async.db.Connection
import com.github.mauricio.async.db.{ QueryResult => DBQueryResult }
import com.github.mauricio.async.db.RowData
import com.github.mauricio.async.db.pool.ObjectFactory
import com.typesafe.scalalogging.StrictLogging
import io.getquill.naming.NamingStrategy
import io.getquill.sources.sql.SqlSource
import io.getquill.sources.sql.idiom.MySQLDialect
import io.getquill.sources.sql.idiom.SqlIdiom
import language.experimental.macros
import io.getquill.quotation.Quoted
import io.getquill.sources.sql.SqlSourceMacro
import io.getquill.sources.BindedStatementBuilder

class AsyncSource[D <: SqlIdiom, N <: NamingStrategy, C <: Connection](config: AsyncSourceConfig[D, N, C])
  extends SqlSource[D, N, RowData, BindedStatementBuilder[List[Any]]]
  with Decoders
  with Encoders
  with StrictLogging {

  type QueryResult[T] = Future[List[T]]
  type ActionResult[T] = Future[DBQueryResult]
  type BatchedActionResult[T] = Future[List[DBQueryResult]]

  class ActionApply[T](f: List[T] => Future[List[DBQueryResult]])(implicit ec: ExecutionContext)
    extends Function1[List[T], Future[List[DBQueryResult]]] {
    def apply(params: List[T]) = f(params)
    def apply(param: T) = f(List(param)).map(_.head)
  }

  private val pool = config.pool

  override def close = {
    Await.result(pool.close, Duration.Inf)
    ()
  }

  private def withConnection[T](f: Connection => Future[T])(implicit ec: ExecutionContext) =
    ec match {
      case TransactionalExecutionContext(ec, conn) => f(conn)
      case other                                   => f(pool)
    }

  def probe(sql: String) =
    Try {
      Await.result(pool.sendQuery(sql), Duration.Inf)
    }

  def transaction[T](f: TransactionalExecutionContext => Future[T])(implicit ec: ExecutionContext) =
    pool.inTransaction { c =>
      f(TransactionalExecutionContext(ec, c))
    }

  def execute(sql: String, generated: Option[String])(implicit ec: ExecutionContext) = {
    logger.info(sql)
    withConnection(_.sendQuery(sql))
  }

  def execute[T](sql: String, bindParams: T => BindedStatementBuilder[List[Any]] => BindedStatementBuilder[List[Any]], generated: Option[String])(implicit ec: ExecutionContext): ActionApply[T] = {
    def run(values: List[T]): Future[List[DBQueryResult]] =
      values match {
        case Nil =>
          Future.successful(List())
        case value :: tail =>
          val (expanded, params) = bindParams(value)(new BindedStatementBuilder).build(sql)
          logger.info(expanded)
          withConnection(_.sendPreparedStatement(sql, params(List())))
            .flatMap(r => run(tail).map(r +: _))
      }
    new ActionApply(run _)
  }

  def query[T](sql: String, bind: BindedStatementBuilder[List[Any]] => BindedStatementBuilder[List[Any]], extractor: RowData => T)(implicit ec: ExecutionContext) = {
    val (expanded, params) = bind(new BindedStatementBuilder).build(sql)
    logger.info(expanded)
    withConnection(_.sendPreparedStatement(expanded, params(List()))).map {
      _.rows match {
        case Some(rows) => rows.map(extractor).toList
        case None       => List()
      }
    }
  }
}
