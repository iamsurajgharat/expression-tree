package com.surajgharat.expressiontree
import scala.util.Try

trait CExpression {
  def eval(request: ExpressionRequest): Try[Any]
}

object CExpression {
  def addOpr(e1: CNumberExpression, e2: CNumberExpression): CNumberExpression =
    new CNumberExpression(request => {
      e1.eval(request).flatMap(n1 => e2.eval(request).map(n2 => n1 + n2))
    })

  def addOpr2(
      e1: CExpression2[Float],
      e2: CExpression2[Float]
  ): CExpression2[Float] =
    binaryOpr[Float, Float, Float](e1, e2, (x, y) => x + y)

  def subtractOpr(
      e1: CExpression2[Float],
      e2: CExpression2[Float]
  ): CExpression2[Float] =
    binaryOpr[Float, Float, Float](e1, e2, (x, y) => x - y)

  def multiplyOpr(
      e1: CExpression2[Float],
      e2: CExpression2[Float]
  ): CExpression2[Float] =
    binaryOpr[Float, Float, Float](e1, e2, (x, y) => x * y)

  def divideOpr(
      e1: CExpression2[Float],
      e2: CExpression2[Float]
  ): CExpression2[Float] =
    binaryOpr[Float, Float, Float](e1, e2, (x, y) => x / y)

  def moduleOpr(
      e1: CExpression2[Float],
      e2: CExpression2[Float]
  ): CExpression2[Float] =
    binaryOpr[Float, Float, Float](e1, e2, (x, y) => x % y)

  def gteOpr(
      e1: CExpression2[Float],
      e2: CExpression2[Float]
  ): CExpression2[Boolean] =
    binaryOpr[Float, Float, Boolean](e1, e2, (x, y) => x >= y)

  def gtOpr(
      e1: CExpression2[Float],
      e2: CExpression2[Float]
  ): CExpression2[Boolean] =
    binaryOpr[Float, Float, Boolean](e1, e2, (x, y) => x > y)

  def lteOpr(
      e1: CExpression2[Float],
      e2: CExpression2[Float]
  ): CExpression2[Boolean] =
    binaryOpr[Float, Float, Boolean](e1, e2, (x, y) => x <= y)

  def ltOpr(
      e1: CExpression2[Float],
      e2: CExpression2[Float]
  ): CExpression2[Boolean] =
    binaryOpr[Float, Float, Boolean](e1, e2, (x, y) => x < y)

  def eqOpr(
      e1: CExpression2[Float],
      e2: CExpression2[Float]
  ): CExpression2[Boolean] =
    binaryOpr[Float, Float, Boolean](e1, e2, (x, y) => x == y)

  def neqOpr(
      e1: CExpression2[Float],
      e2: CExpression2[Float]
  ): CExpression2[Boolean] =
    binaryOpr[Float, Float, Boolean](e1, e2, (x, y) => x != y)

  def andOpr(
      e1: CExpression2[Boolean],
      e2: CExpression2[Boolean]
  ): CExpression2[Boolean] =
    binaryOpr[Boolean, Boolean, Boolean](e1, e2, (x, y) => x && y)

  def orOpr(
      e1: CExpression2[Boolean],
      e2: CExpression2[Boolean]
  ): CExpression2[Boolean] =
    binaryOpr[Boolean, Boolean, Boolean](e1, e2, (x, y) => x || y)

  def negateOpr(
      e1: CExpression2[Boolean]
  ): CExpression2[Boolean] = unaryOpr[Boolean, Boolean](e1, x => !x)

  private def binaryOpr[T1, T2, T3](
      e1: CExpression2[T1],
      e2: CExpression2[T2],
      handler: (T1, T2) => T3
  ): CExpression2[T3] =
    new CExpression2[T3](request => {
      for {
        n1 <- e1.eval(request)
        n2 <- e2.eval(request)
      } yield handler(n1, n2)
    })

  private def unaryOpr[T1, T2](
      e1: CExpression2[T1],
      handler: T1 => T2
  ): CExpression2[T2] =
    new CExpression2[T2](request => {
      for {
        n1 <- e1.eval(request)
      } yield handler(n1)
    })

}

class CNumberExpression(func: ExpressionRequest => Try[Float])
    extends CExpression {
  def eval(request: ExpressionRequest): Try[Float] = func(request)
}

class CExpression2[T](func: ExpressionRequest => Try[T]) extends CExpression {
  def eval(request: ExpressionRequest): Try[T] = func(request)
}
