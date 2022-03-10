package io.github.iamsurajgharat
package expressiontree
import org.scalatest.matchers.should.Matchers
import scala.util.Failure
import scala.util.Success
import org.scalatest.flatspec.AnyFlatSpec
import io.github.iamsurajgharat.expressiontree.expressiontree._

class AddTests extends AnyFlatSpec with Matchers {
    val record : Record = new RecordImpl(
        Map(
            "field1" -> Number(10)
        )
    )

    "AddOpr" should "return none for 10 + null" in {
        // e1 is 10, e2 is undefined
        val e1 = SExpression.variable("field1", DataType.Number)
        val e2 = SExpression.variable("field9", DataType.Number)
        val e3 = SExpression.operation(SExpOpType.AddOpr, e1, e2)

        // act
        val result = e3.compile().eval(ExpressionRequest(record, null))

        // assure
        result.isFailure shouldBe true
    }
}