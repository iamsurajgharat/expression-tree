package io.github.iamsurajgharat.expressiontree
package expressiontree

import org.scalatest.flatspec.AnyFlatSpec
import io.github.iamsurajgharat.expressiontree.expressiontree._
import scala.util.Success
import org.scalatest.matchers.should.Matchers

class DayTests extends AnyFlatSpec with Matchers {
    val record = new RecordImpl(
        Map(
            "field1" -> expressiontree.RNumber(25),
            "field21" -> RText("Tony Stark"),
            "field31" -> RDate(new org.joda.time.LocalDate(2008, 10,15))
        )
    )

    "DAY" should """return 15 for 2008-10-15""" in {
        val e1 = SExpression.variable("field31", DataType.Date)
        val e3 = SExpression.createFuncDay(e1).get

        // act
        val result = e3.compile().eval(ExpressionRequest(record, null))

        // assure
        result shouldBe Success(Some(15))
    }

    it should """return none for blank field""" in {
        val e1 = SExpression.variable("field32", DataType.Date)
        val e3 = SExpression.createFuncDay(e1).get

        // act
        val result = e3.compile().eval(ExpressionRequest(record, null))

        // assure
        result shouldBe Success(None)
    }
}