package io.getquill.norm

import io.getquill.ast._

private[norm] case class State(tableAlias: Option[String] = None, properties: List[PropertyAlias] = List(), generated: Option[String] = None)

private[norm] case class MergeSchemaWithEntity(state: State) extends StatefulTransformer[State] {
  override def apply(e: SchemaDefinition): (SchemaDefinition, StatefulTransformer[State]) =
    e match {
      case Table(a, b) =>
        val (at, att) = apply(a)
        (Table(at, b), MergeSchemaWithEntity(att.state.copy(tableAlias = Some(b))))
      case Columns(a, b) =>
        val (at, att) = apply(a)
        (Columns(at, b), MergeSchemaWithEntity(att.state.copy(properties = b)))
      case Generated(a, b, c) =>
        val (at, att) = apply(a)
        val (ct, ctt) = att.apply(c)
        (Generated(at, b, ct), MergeSchemaWithEntity(ctt.state.copy(generated = singleProperty(c))))
    }

  private def singleProperty(ast: Ast) = ast match {
    case Property(Ident(a), b) => Some(b)
    case _                     => None
  }
}

private[norm] case class RemoveSchema() extends StatelessTransformer {
  override def apply(e: Query): Query =
    e match {
      case Schema(query: Query, _, _) => query
      case other                      => super.apply(other)
    }
}

private[norm] case class ModifyEntity(state: State) extends StatelessTransformer {
  override def apply(e: Query): Query =
    e match {
      case e: Entity =>
        e.copy(
          alias = ifNonEmpty(state.tableAlias, e.alias),
          properties = ifNonEmpty(state.properties, e.properties),
          generated = ifNonEmpty(state.generated, e.generated)
        )
      case other => super.apply(other)
    }

  private def ifNonEmpty[T](list: List[T], ifEmpty: List[T]) = if (list.nonEmpty) list else ifEmpty
  private def ifNonEmpty[T](list: Option[T], ifEmpty: Option[T]) = if (list.nonEmpty) list else ifEmpty
}

object MergeSchemaWithEntity {
  def apply(q: Ast) =
    new MergeSchemaWithEntity(State())(q) match {
      case (q, s) =>
        RemoveSchema()(ModifyEntity(s.state)(q))
    }
}