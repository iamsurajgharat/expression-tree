package io.github.iamsurajgharat
package expressiontree

import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import scala.util.Success
import io.github.iamsurajgharat.expressiontree.expressiontree._

class EqualsTests extends AnyFlatSpec with Matchers{
    val record:Record = new RecordImpl(
                                        Map(
                                            "field1" -> Number(10),
                                            "field11" -> Bool(true),
                                            "field12" -> Bool(false),
                                            "field21" -> Text("Avengers")
                                        )
                                    )

    "The Equals" should "return true for 10 == 10" in {
        // arrange
        val e1 = SExpression.variable("field1", DataType.Number)
        val e2 = SExpression.constant(10)
        val e3 = SExpression.operation(SExpOpType.EqOpr, e1, e2)

        val req = ExpressionRequest(record, null)

        // act
        val result = e3.compile().eval(req)

        // assure
        result shouldBe Success(Some(true))

    }

    it should "return true for 10 == 10.0" in {
        // arrange
        val e1 = SExpression.variable("field1", DataType.Number)
        val e2 = SExpression.constant(10.0f)
        val e3 = SExpression.operation(SExpOpType.EqOpr, e1, e2)

        val req = ExpressionRequest(record, null)

        // act
        val result = e3.compile().eval(req)

        // assure
        result shouldBe Success(Some(true))
    }

    it should "return true for null == null" in {
        // arrange
        val e1 = SExpression.variable("field2", DataType.Number)
        val e2 = SExpression.constantNull
        val e3 = SExpression.operation(SExpOpType.EqOpr, e1, e2)

        val req = ExpressionRequest(record, null)

        // act
        val result = e3.compile().eval(req)

        // assure
        result shouldBe Success(Some(true))
    }

    it should "return false for 10 == \"10\"" in {
        // arrange
        val e1 = SExpression.variable("field1", DataType.Number)
        val e2 = SExpression.constant("10")
        val e3 = SExpression.operation(SExpOpType.EqOpr, e1, e2)

        val req = ExpressionRequest(record, null)

        // act
        val result = e3.compile().eval(req)

        // assure
        result shouldBe Success(Some(false))
    }

    it should "return false for 10 == true " in {
        // arrange
        val e1 = SExpression.variable("field1", DataType.Number)
        val e2 = SExpression.constant(true)
        val e3 = SExpression.operation(SExpOpType.EqOpr, e1, e2)

        val req = ExpressionRequest(record, null)

        // act
        val result = e3.compile().eval(req)

        // assure
        result shouldBe Success(Some(false))
    }

    it should "return true for true == true " in {
        // arrange
        val e1 = SExpression.variable("field11", DataType.Bool)
        val e2 = SExpression.constant(true)
        val e3 = SExpression.operation(SExpOpType.EqOpr, e1, e2)

        val req = ExpressionRequest(record, null)

        // act
        val result = e3.compile().eval(req)

        // assure
        result shouldBe Success(Some(true))
    }

    it should "return true for false == false " in {
        // arrange
        val e1 = SExpression.variable("field11", DataType.Bool)
        val e2 = SExpression.constant(false)
        val e3 = SExpression.operation(SExpOpType.EqOpr, e1, e2)

        val req = ExpressionRequest(record, null)

        // act
        val result = e3.compile().eval(req)

        // assure
        result shouldBe Success(Some(false))
    }

    it should "return false for false == true " in {
        // arrange
        val e1 = SExpression.variable("field12", DataType.Bool)
        val e2 = SExpression.constant(true)
        val e3 = SExpression.operation(SExpOpType.EqOpr, e1, e2)

        val req = ExpressionRequest(record, null)

        // act
        val result = e3.compile().eval(req)

        // assure
        result shouldBe Success(Some(false))
    }

    it should """return true for "Avengers" == "Avengers" """ in {
        // arrange
        val e1 = SExpression.variable("field21", DataType.Text)
        val e2 = SExpression.constant("Avengers")
        val e3 = SExpression.operation(SExpOpType.EqOpr, e1, e2)

        val req = ExpressionRequest(record, null)

        // act
        val result = e3.compile().eval(req)

        // assure
        result shouldBe Success(Some(true))
    }
}