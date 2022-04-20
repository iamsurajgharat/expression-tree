package io.github.iamsurajgharat

package expressiontree

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.util.Success
import io.github.iamsurajgharat.expressiontree.expressiontree._

class NotEqualsTests extends AnyFlatSpec with Matchers {
    val record : Record = new RecordImpl(
        Map(
            "field1" -> Number(10),
            "field11" -> Bool(true),
            "field12" -> Bool(false),
            "field21" -> Text("Harry Potter"),
            "field22" -> Text("ABC")
        )
    )

    "The NotEquals" should "return true for 10 != 20" in {
        // arrange
        val e1 = SExpression.variable("field1", DataType.Number)
        val e2 = SExpression.constant(20)
        val e3 = SExpression.operation(SExpOpType.NeqOpr, e1, e2)

        val req = ExpressionRequest(record, null)

        // act
        val result = e3.compile().eval(req)

        // assure
        result shouldBe Success(Some(true))
    }

    it should "return false for 10 != 10" in {
        // arrange
        val e1 = SExpression.variable("field1", DataType.Number)
        val e2 = SExpression.constant(10)
        val e3 = SExpression.operation(SExpOpType.NeqOpr, e1, e2)

        val req = ExpressionRequest(record, null)

        // act
        val result = e3.compile().eval(req)

        // assure
        result shouldBe Success(Some(false))
    }

    it should "return true for true != false" in {
        // arrange
        val e1 = SExpression.variable("field11", DataType.Bool)
        val e2 = SExpression.constant(false)
        val e3 = SExpression.operation(SExpOpType.NeqOpr, e1, e2)

        val req = ExpressionRequest(record, null)

        // act
        val result = e3.compile().eval(req)

        // assure
        result shouldBe Success(Some(true))
    }

    it should "return false for false != false" in {
        // arrange
        val e1 = SExpression.variable("field12", DataType.Bool)
        val e2 = SExpression.constant(false)
        val e3 = SExpression.operation(SExpOpType.NeqOpr, e1, e2)

        val req = ExpressionRequest(record, null)

        // act
        val result = e3.compile().eval(req)

        // assure
        result shouldBe Success(Some(false))
    }

    it should """return true for "ABC" != "abc"""" in {
        // arrange
        val e1 = SExpression.variable("field22", DataType.Text)
        val e2 = SExpression.constant("abc")
        val e3 = SExpression.operation(SExpOpType.NeqOpr, e1, e2)

        val req = ExpressionRequest(record, null)

        // act
        val result = e3.compile().eval(req)

        // assure
        result shouldBe Success(Some(true))
    }

    it should """return false for "ABC" != "ABC"""" in {
        // arrange
        val e1 = SExpression.variable("field22", DataType.Text)
        val e2 = SExpression.constant("ABC")
        val e3 = SExpression.operation(SExpOpType.NeqOpr, e1, e2)

        val req = ExpressionRequest(record, null)

        // act
        val result = e3.compile().eval(req)

        // assure
        result shouldBe Success(Some(false))
    }

    it should """return true for "ABC" != null""" in {
        // arrange
        val e1 = SExpression.variable("field22", DataType.Text)
        val e2 = SExpression.constantNull
        val e3 = SExpression.operation(SExpOpType.NeqOpr, e1, e2)

        val req = ExpressionRequest(record, null)

        // act
        val result = e3.compile().eval(req)

        // assure
        result shouldBe Success(Some(true))
    }

    it should "return false for null != null" in {
        // arrange
        val e1 = SExpression.variable("field101", DataType.Text)
        val e2 = SExpression.constantNull
        val e3 = SExpression.operation(SExpOpType.NeqOpr, e1, e2)

        val req = ExpressionRequest(record, null)

        // act
        val result = e3.compile().eval(req)

        // assure
        result shouldBe Success(Some(false))
    }
}