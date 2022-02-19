package com.surajgharat.expressiontree

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.util.Success

class CExpressionTests extends AnyFlatSpec with Matchers {
    val schema = new RecordSchema(Map(
        "field1" -> DataType.Text,
        "field2" -> DataType.Number,
        "field3" -> DataType.Bool
    ))

    val record1 = new RecordImpl().
                        put("field1", "ABCD").
                        put("field2", 234f).
                        put("field3", true)

    "A constant CExpression" should "return correct number value" in {
        val e1 = SExpression.constant(10)
        val result = e1.compile().eval(null)

        // assure
        result shouldBe Success(Some(10f))
    }

    it should "return correct bool value" in {
        val e1 = SExpression.constant(true)
        val result = e1.compile().eval(null)

        // assure
        result shouldBe Success(Some(true))
    }

    it should "return correct test value" in {
        val e1 = SExpression.constant("Harry Potter")
        val result = e1.compile().eval(null)

        // assure
        result shouldBe Success(Some("Harry Potter"))
    }

    "A variable CExpression" should "return correct number value" in {
        val e1 = SExpression.variable("field2", DataType.Number)
        val result = e1.compile().eval(ExpressionRequest(record1, null))

        // assure
        result shouldBe Success(Some(234f))
    }

    it should "return correct text value" in {
        val e1 = SExpression.variable("field1", DataType.Text)
        val result = e1.compile().eval(ExpressionRequest(record1, null))

        // assure
        result shouldBe Success(Some("ABCD"))
    }

    it should "return correct bool value" in {
        val e1 = SExpression.variable("field3", DataType.Bool)
        val result = e1.compile().eval(ExpressionRequest(record1, null))

        // assure
        result shouldBe Success(Some(true))
    }

    "The AddOpr" should "return correct sum" in {
        val e1 = SExpression.variable("field2", DataType.Number)
        val e2 = SExpression.constant(55)
        val e3 = SExpression.operation(SExpOpType.AddOpr, e1, e2)
        val result = e3.compile().eval(ExpressionRequest(record1, null))

        // assure
        result shouldBe Success(Some(234f + 55))
    }

    it should "return error if the operand is not number" in {
        val e1 = SExpression.variable("field1", DataType.Text)
        val e2 = SExpression.constant(10)
        val e3 = SExpression.operation(SExpOpType.AddOpr, e1, e2)
        val result = e3.compile().eval(ExpressionRequest(record1, null))

        // assure
        val failure = result.asInstanceOf[scala.util.Failure[Throwable]]
        failure should not be null
        failure.exception.getMessage() should include ("while evaluating binary op [+]")
    }

    it should "return error if the operand is blank/null" in {
        val e1 = SExpression.variable("field5", DataType.Number)
        val e2 = SExpression.constant(10)
        val e3 = SExpression.operation(SExpOpType.AddOpr, e1, e2)
        val result = e3.compile().eval(ExpressionRequest(record1, null))

        // assure
        val failure = result.asInstanceOf[scala.util.Failure[Throwable]]
        failure should not be null
        failure.exception.getMessage() should include ("while evaluating binary op [+]")
    }
    
    "The SubtractOpr" should "return correct sum" in {
        val e1 = SExpression.variable("field2", DataType.Number)
        val e2 = SExpression.constant(55)
        val e3 = SExpression.operation(SExpOpType.SubtractOpr, e1, e2)
        val result = e3.compile().eval(ExpressionRequest(record1, null))

        // assure
        result shouldBe Success(Some(234f - 55))
    }
}