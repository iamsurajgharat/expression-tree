package com.surajgharat.expressiontree
import scala.util.Try
import scala.util.Success
import scala.util.Failure

trait CExpression {
  def eval(request: ExpressionRequest): Try[Any]
}

object CExpression {
  def addOpr(
      e1: CExpressionImpl[Float],
      e2: CExpressionImpl[Float]
  ): CExpressionImpl[Float] =
    binaryOpr[Float, Float, Float](e1, e2, (x, y) => x + y, "+")

  def subtractOpr(
      e1: CExpressionImpl[Float],
      e2: CExpressionImpl[Float]
  ): CExpressionImpl[Float] =
    binaryOpr[Float, Float, Float](e1, e2, (x, y) => x - y, "-")

  def multiplyOpr(
      e1: CExpressionImpl[Float],
      e2: CExpressionImpl[Float]
  ): CExpressionImpl[Float] =
    binaryOpr[Float, Float, Float](e1, e2, (x, y) => x * y, "*")

  def divideOpr(
      e1: CExpressionImpl[Float],
      e2: CExpressionImpl[Float]
  ): CExpressionImpl[Float] =
    binaryOpr[Float, Float, Float](e1, e2, (x, y) => x / y, "/")

  def moduleOpr(
      e1: CExpressionImpl[Float],
      e2: CExpressionImpl[Float]
  ): CExpressionImpl[Float] =
    binaryOpr[Float, Float, Float](e1, e2, (x, y) => x % y, "%")

  def gteOpr(
      e1: CExpressionImpl[Float],
      e2: CExpressionImpl[Float]
  ): CExpressionImpl[Boolean] =
    binaryOpr[Float, Float, Boolean](e1, e2, (x, y) => x >= y, ">=")

  def gtOpr(
      e1: CExpressionImpl[Float],
      e2: CExpressionImpl[Float]
  ): CExpressionImpl[Boolean] =
    binaryOpr[Float, Float, Boolean](e1, e2, (x, y) => x > y, ">")

  def lteOpr(
      e1: CExpressionImpl[Float],
      e2: CExpressionImpl[Float]
  ): CExpressionImpl[Boolean] =
    binaryOpr[Float, Float, Boolean](e1, e2, (x, y) => x <= y, "<=")

  def ltOpr(
      e1: CExpressionImpl[Float],
      e2: CExpressionImpl[Float]
  ): CExpressionImpl[Boolean] =
    binaryOpr[Float, Float, Boolean](e1, e2, (x, y) => x < y, "<")

  def eqOpr(
      e1: CExpressionImpl[Float],
      e2: CExpressionImpl[Float]
  ): CExpressionImpl[Boolean] =
    binaryOpr[Float, Float, Boolean](e1, e2, (x, y) => x == y, "==")

  def neqOpr(
      e1: CExpressionImpl[Float],
      e2: CExpressionImpl[Float]
  ): CExpressionImpl[Boolean] =
    binaryOpr[Float, Float, Boolean](e1, e2, (x, y) => x != y, "!=")

  def andOpr(
      e1: CExpressionImpl[Boolean],
      e2: CExpressionImpl[Boolean]
  ): CExpressionImpl[Boolean] =
    binaryOpr[Boolean, Boolean, Boolean](e1, e2, (x, y) => x && y, "&&")

  def orOpr(
      e1: CExpressionImpl[Boolean],
      e2: CExpressionImpl[Boolean]
  ): CExpressionImpl[Boolean] =
    binaryOpr[Boolean, Boolean, Boolean](e1, e2, (x, y) => x || y, "||")

  def negateOpr(
      e1: CExpressionImpl[Boolean]
  ): CExpressionImpl[Boolean] = unaryOpr[Boolean, Boolean](e1, x => !x)

  private def binaryOpr[T1, T2, T3](
      e1: CExpressionImpl[T1],
      e2: CExpressionImpl[T2],
      handler: (T1, T2) => T3,
      name: String
  ): CExpressionImpl[T3] =
    new CExpressionImpl[T3](request => {
      val r2 = for {
        n21 <- e1.eval(request)
        n22 <- e2.eval(request)
      } yield handler(n21, n22)

      /*val result = Try{
        val n1 = e1.eval(request)
        val n2 = e2.eval(request)
        val n11 = n1.get
        val n12 = n2.get
        val n3 = handler(n11,n12)
        n3
      }*/

      r2 match {
        case Success(value) => r2
        case Failure(exception) if exception.isInstanceOf[ClassCastException] =>
          Failure(
            new Exception(
              s"Error ${exception.getMessage()} while evaluation binary op ${name}"
            )
          )
      }
    })

  private def unaryOpr[T1, T2](
      e1: CExpressionImpl[T1],
      handler: T1 => T2
  ): CExpressionImpl[T2] =
    new CExpressionImpl[T2](request => {
      for {
        n1 <- e1.eval(request)
      } yield handler(n1)
    })

}

class CExpressionImpl[T](func: ExpressionRequest => Try[T])
    extends CExpression {
  def eval(request: ExpressionRequest): Try[T] = {
    val result = func(request)
    result
  }
}
