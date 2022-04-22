package io.github.iamsurajgharat
package expressiontree

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.util.Success
import io.github.iamsurajgharat.expressiontree.expressiontree._

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

    it should "create SExpression for given date" in {
        // act
        val result = SExpression.constant(new org.joda.time.LocalDate(2022, 10, 15))

        // assure
        result.value shouldBe Some(new org.joda.time.LocalDate(2022, 10, 15))
    }

    it should "create SExpression for given datetime" in {
        // arrange
        val time1 = new org.joda.time.DateTime(2022, 10, 15, 14, 45, 55, 788, org.joda.time.DateTimeZone.UTC)

        // act
        val result = SExpression.constant(time1)

        // assure
        result.value shouldBe Some(time1)
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

    it should "create CExpression for given date SExpression" in {
        // arrange
        var date1 = new org.joda.time.LocalDate(2022, 10, 15)
        val e1 = SExpression.constant(date1)

        // act
        val result = e1.compile()

        // assure
        result.eval(null) shouldBe Success(Some(date1))
    }

    it should "create CExpression for given datetime SExpression" in {
        // arrange
        var date1 = new org.joda.time.DateTime(2022, 10, 15, 14, 45, 56)
        val e1 = SExpression.constant(date1)

        // act
        val result = e1.compile()

        // assure
        result.eval(null) shouldBe Success(Some(date1))
    }

    "startsWith" should "create CExpression for string startsWith function" in {
        val s1 = SExpression.constant("Tony Stark")
        val s2 = SExpression.constant("T")

        // act
        val result = SExpression.createFuncStartsWith(s1, s2)

        // assure
        result.isSuccess shouldBe true
        result.get.otype shouldBe Some(SExpOpType.StartsWithFun)
    }

    it should "fail if any of argument is not string" in {
        val s1 = SExpression.constant("Tony Stark")
        val s2 = SExpression.constant(34)

        // act
        val result = SExpression.createFuncStartsWith(s1, s2)

        // assure
        result.isSuccess shouldBe false
    }

    "createFuncIf" should "return something nice for something relatively nice " in {
        // arrange
        val s1 = SExpression.constant(true)
        val s2 = SExpression.constant(10)
        val s3 = SExpression.constant(20)

        // act
        val result = SExpression.createFuncIf(s1, s2, s3)

        // assure
        result.isSuccess shouldBe true
        result.map(x => {
            x.rtype shouldBe DataType.Number 
        })
    }
}