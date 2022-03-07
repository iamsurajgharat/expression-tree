package com.surajgharat.expressiontree

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.util.Success

class SExpressionTests extends AnyFlatSpec with Matchers {
    "Constant" should "create SExpression for given integer" in {
        // act
        val result = SExpression.constant(10)

        // assure
        result.rtype shouldBe DataType.Number
    }

    it should "create SExpression for given float" in {
        // act
        val result = SExpression.constant(10.12f)

        // assure
        result.value shouldBe Some(10.12f)
    }

    it should "create SExpression for given long" in {
        // act
        val result = SExpression.constant(100l)

        // assure
        result.value shouldBe Some(100f)
        result.etype shouldBe SExpType.Constant
    }

    it should "create SExpression for given bool" in {
        // act
        val result = SExpression.constant(true)

        // assure
        result.value shouldBe Some(true)
    }

    it should "create SExpression for given string" in {
        // act
        val result = SExpression.constant("Hocus pocus!")

        // assure
        result.value shouldBe Some("Hocus pocus!")
    }

    "Variable" should "create SExpression for path" in {
        // act
        var result = SExpression.variable("field1.field2", DataType.Number)

        // assure
        result.path shouldBe Some("field1.field2")
        result.rtype shouldBe DataType.Number
    }

    "constantNull" should "create SExpression for null constant" in {
        // act
        val result = SExpression.constantNull

        // assure
        result.rtype shouldBe DataType.Null
    }

    "operation" should "create SExpression for given arguments" in {
        // act
        val result = SExpression.operation(SExpOpType.AddOpr, SExpression.constant(10), SExpression.constant(20))

        // assure
        result.rtype shouldBe DataType.Number
        result.args should have size 2
        result.etype shouldBe SExpType.Operation
    }

    "compile" should "create CExpression for given simple SExpression" in {
        // arrange
        val e1 = SExpression.constant(10)

        // act
        val result = e1.compile()

        // assure
        result.eval(null) shouldBe Success(Some(10))
    }
}