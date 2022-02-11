package example
import com.surajgharat.expressiontree._
object Hello extends Greeting with App {
  import DataType._
  var recordSchema = RecordSchema(Map("key1" -> Number))
  val e1 = SExpression.constant(6)
  val e2 = SExpression.constant(50)
  val e3 = SExpression.operation(SExpOpType.AddOpr, e1, e2)
  val e4 = SExpression.constant(55)
  val e5 = SExpression.operation(SExpOpType.GtOpr, e3, e4)
  println(java.time.LocalDateTime.now())
  val ce = e5.compile(recordSchema)
  println(java.time.LocalDateTime.now())
  println(ce.eval(null))
  println(java.time.LocalDateTime.now())
}

trait Greeting {
  lazy val greeting: String = "hello"
}
