package io.github.iamsurajgharat.expressiontree
package expressiontree

import org.scalatest.flatspec.AnyFlatSpec
import io.github.iamsurajgharat.expressiontree.expressiontree._
import scala.util.Success
import org.scalatest.matchers.should.Matchers

class HourTests extends AnyFlatSpec with Matchers {
    val record = new RecordImpl(
        Map(
            "field41" -> RDateTime(new org.joda.time.DateTime(2022, 10, 15, 13, 56, 45))
        )
    )

    "HOUR" should "return 13 for 2008-10-15T13:56:45" in {
        val e1 = SExpression.variable("field41", DataType.Datetime)
        val e3 = SExpression.createFuncHour(e1).get

        // act
        val result = e3.compile().eval(ExpressionRequest(record, null))

        // assure
        result shouldBe Success(Some(13))
    }

    it should "return 17 for 2008-10-15T17:56:45" in {
        val e1 = SExpression.constant(new org.joda.time.DateTime(2022, 10, 15, 17, 56, 45))
        val e3 = SExpression.createFuncHour(e1).get

        // act
        val result = e3.compile().eval(ExpressionRequest(record, null))

        // assure
        result shouldBe Success(Some(17))
    }

    it should "return none for blank field" in {
        val e1 = SExpression.variable("field42", DataType.Datetime)
        val e3 = SExpression.createFuncHour(e1).get

        // act
        val result = e3.compile().eval(ExpressionRequest(record, null))

        // assure
        result shouldBe Success(None)
    }
}