package io.github.iamsurajgharat.expressiontree
package expressiontree

import org.scalatest.flatspec.AnyFlatSpec
import io.github.iamsurajgharat.expressiontree.expressiontree._
import scala.util.Success
import org.scalatest.matchers.should.Matchers

class IfTests extends AnyFlatSpec with Matchers {
    val record = new RecordImpl(
        Map(
            "field1" -> RBool(true),
            "field2" -> RBool(false),
            "field11" -> RText("DC"),
            "field12" -> RText("Marvel")
        )
    )

    "IF" should "return 10 for IF(true, 10, 20)" in {
        val e1 = SExpression.constant(true)
        val e2 = SExpression.constant(10)
        val e3 = SExpression.constant(20)
        val e4 = SExpression.createFuncIf(e1, e2, e3).get

        // act
        val result = e4.compile().eval(ExpressionRequest(record, null)).get

        // assure
        result shouldBe Some(10)
    }

    it should """return "Marvel" for IF(field2, "DC", "Marvel")""" in {
        val e1 = SExpression.variable("field2", DataType.Bool)
        val e2 = SExpression.variable("field11", DataType.Text)
        val e3 = SExpression.variable("field12", DataType.Text)
        val e4 = SExpression.createFuncIf(e1, e2, e3).get

        // act
        val result = e4.compile().eval(ExpressionRequest(record, null)).get

        // assure
        result shouldBe Some("Marvel")
    }
}