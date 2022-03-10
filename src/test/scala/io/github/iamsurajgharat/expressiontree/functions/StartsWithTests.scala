package io.github.iamsurajgharat.expressiontree
package expressiontree

import org.scalatest.flatspec.AnyFlatSpec
import io.github.iamsurajgharat.expressiontree.expressiontree._
import scala.util.Success
import org.scalatest.matchers.should.Matchers

class StartsWithTests extends AnyFlatSpec with Matchers {
    val record = new RecordImpl(
        Map(
            "field1" -> expressiontree.Number(25),
            "field21" -> Text("Tony Stark")
        )
    )

    "STARTSWITH" should """return true for "Tony Stark" and "T"""" in {
        val e1 = SExpression.constant("Tony Stark")
        val e2 = SExpression.constant("T")
        val e3 = SExpression.startsWith(e1, e2).get

        // act
        val result = e3.compile().eval(null)

        // assure
        result shouldBe Success(Some(true))
    }

    it should "return false if any param is undefined" in {
        val e1 = SExpression.variable("field22", DataType.Text)
        val e2 = SExpression.constant("T")
        val e3 = SExpression.startsWith(e1, e2).get

        // act
        val result = e3.compile().eval(ExpressionRequest(record, null))

        // assure
        result shouldBe Success(Some(false))
    }

    it should "return true for big on1" in {
        val e1 = SExpression.variable("field1", DataType.Number)
        val e2 = SExpression.constant(15)
        val e3 = SExpression.operation(SExpOpType.GtOpr, e1, e2)
        val e4 = SExpression.variable("field21", DataType.Text)
        val e5 = SExpression.constant("Tony")
        val e6 = SExpression.startsWith(e4, e5).get
        val e7 = SExpression.operation(SExpOpType.AndOpr, e3, e6)

        // act
        val result = e7.compile().eval(ExpressionRequest(record, null))

        // assure
        result shouldBe Success(Some(true))
    }
}