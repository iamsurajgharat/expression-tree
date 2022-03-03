package com.surajgharat.expressiontree

import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import scala.util.Success
import com.surajgharat.expressiontree.RecordImpl

class EqualsOperatorTests extends AnyFlatSpec with Matchers{

    "The Equals" should "return true for two same integers" in {
        // arrange
        val e1 = SExpression.variable("field1", DataType.Number)
        val e2 = SExpression.constant(10)
        val e3 = SExpression.operation(SExpOpType.EqOpr, e1, e2)

        val record:Record = new RecordImpl(Map("field1" -> Number(10)))
        val req = ExpressionRequest(record, null)

        // act
        val result = e3.compile().eval(req)

        // assure
        result shouldBe Success(Some(true))

    }
}