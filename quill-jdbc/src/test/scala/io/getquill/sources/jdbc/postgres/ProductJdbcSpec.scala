package io.getquill.sources.jdbc.postgres

import io.getquill._

class ProductJdbcSpec extends Spec {

  override def beforeAll = {
    testPostgresDB.run(quote(query[Product].delete))
    ()
  }

  case class Product(id: Int, description: String, sku: Long)

  val product = quote {
    query[Product].schema(_.generated(_.id))
  }

  val productInsert = quote {
    query[Product].schema(_.generated(_.id)).insert
  }

  val productById = quote {
    (id: Int) => product.filter(_.id == id)
  }

  val productEntries = List(
    Product(0, "Notebook", 1001L),
    Product(0, "Soap", 1002L),
    Product(0, "Pencil", 1003L)
  )

  "Insert product" in {
    val inserted = testPostgresDB.run(productInsert)(productEntries)
    val product = testPostgresDB.run(productById)(inserted(2)).head
    product.description mustEqual productEntries(2).description
  }

}
