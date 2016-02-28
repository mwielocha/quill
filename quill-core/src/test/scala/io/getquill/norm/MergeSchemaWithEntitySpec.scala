package io.getquill.norm

import io.getquill._
import io.getquill.ast.Schema
import io.getquill.ast._

class MergeSchemaWithEntitySpec extends Spec {
  "merge schema with entity" - {
    val q = quote {
      query[TestEntity].schema(s => s.table("test").columns(_.i -> "'i", _.o -> "'o").generated(c => c.i))
    }

    val expectedAst =
      Entity("TestEntity", Some("test"), List(PropertyAlias("i", "'i"), PropertyAlias("o", "'o")), Some("i"))

    MergeSchemaWithEntity(q.ast) mustEqual (expectedAst)
  }

}
