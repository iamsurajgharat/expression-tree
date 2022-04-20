package io.github.iamsurajgharat.expressiontree
package expressiontree

import scala.util.Try
import scala.util.Success
import scala.util.Failure

trait CExpression {
  def eval(request: ExpressionRequest): Try[Option[Any]]
}

object CExpression {
  def constant[T](value: T): CExpressionImpl[T] = new CExpressionImpl[T](_ =>
    Success(if (value != null) Some(value) else None)
  )

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
      e1: CExpressionImpl[Any],
      e2: CExpressionImpl[Any]
  ): CExpressionImpl[Boolean] =
    binaryOptionOpr[Any, Any, Boolean](
      e1,
      e2,
      (x, y) => Some(doEquals(x, y)),
      "=="
    )

  def neqOpr(
      e1: CExpressionImpl[Any],
      e2: CExpressionImpl[Any]
  ): CExpressionImpl[Boolean] =
    binaryOptionOpr[Any, Any, Boolean](
      e1,
      e2,
      (x, y) => Some(!doEquals(x, y)),
      "!="
    )

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
  ): CExpressionImpl[Boolean] = unaryOpr[Boolean, Boolean](e1, x => !x, "!")

  // functions
  def createFuncStartsWith(e1: CExpressionImpl[String], e2:CExpressionImpl[String]) : CExpressionImpl[Boolean] = new CExpressionImpl[Boolean](
    (req) => {

      def execute(a1:Option[String], a2:Option[String]) : Boolean = {
        (a1,a2) match {
          case (Some(p1), Some(p2)) => p1.startsWith(p2)
          case (_, _) => false
        }
      }

      for {
        s1 <- e1.eval(req)
        s2 <- e2.eval(req)
      } yield Some(execute(s1, s2))
    }
  )

  // 
  def createFuncDay(e1: CExpressionImpl[org.joda.time.LocalDate]) : CExpressionImpl[Int] = new CExpressionImpl[Int](
    req => e1.eval(req).map(x => x.map(y => y.getDayOfMonth()))
  )

  private def binaryOpr[T1, T2, T3](
      e1: CExpressionImpl[T1],
      e2: CExpressionImpl[T2],
      handler: (T1, T2) => T3,
      name: String
  ): CExpressionImpl[T3] =
    new CExpressionImpl[T3](request => {

      val result = for {
        n1 <- e1.eval(request)
        n2 <- e2.eval(request)
        r1 <- performBinaryOpWithRequiredCheck(n1, n2, handler, name)
      } yield r1

      result match {
        case Success(value) => result
        case Failure(exception) =>
          Failure(
            new Exception(
              s"Error [${exception.getMessage()}] while evaluating binary op [${name}]"
            )
          )
      }
    })

  private def performBinaryOpWithRequiredCheck[T1, T2, T3](
      p1: Option[T1],
      p2: Option[T2],
      handler: (T1, T2) => T3,
      opName: String
  ): Try[Option[T3]] = {
    (p1, p2) match {
      case (Some(a1), Some(a2)) => Success(Some(handler(a1, a2)))
      case _ =>
        Failure(
          new Exception(
            s"One or both arguments were not provided for binary operation [$opName]"
          )
        )
    }
  }

  private def binaryOptionOpr[T1, T2, T3](
      e1: CExpressionImpl[T1],
      e2: CExpressionImpl[T2],
      handler: (Option[T1], Option[T2]) => Option[T3],
      name: String
  ): CExpressionImpl[T3] =
    new CExpressionImpl[T3](request => {

      val result = for {
        n1 <- e1.eval(request)
        n2 <- e2.eval(request)
      } yield handler(n1, n2)

      result match {
        case Success(value) => result
        case Failure(exception) =>
          Failure(
            new Exception(
              s"Error [${exception.getMessage()}] while evaluating binary op [${name}]"
            )
          )
      }
    })

  private def doEquals[T1, T2](x: T1, y: T2): Boolean =
    (x, y) match {
      case (None, None)         => true
      case (Some(p1), Some(p2)) => p1 == p2
      case _                    => false
    }

  private def unaryOpr[T1, T2](
      e1: CExpressionImpl[T1],
      handler: T1 => T2,
      name: String
  ): CExpressionImpl[T2] =
    new CExpressionImpl[T2](request => {
      for {
        n1 <- e1.eval(request)
        r1 <- performUnaryOpWithRequiredCheck(n1, handler, name)
      } yield r1
    })

  private def performUnaryOpWithRequiredCheck[T1, T2](
      p1: Option[T1],
      handler: (T1) => T2,
      opName: String
  ): Try[Option[T2]] = {
    p1 match {
      case Some(a1) => Success(Some(handler(a1)))
      case _ =>
        Failure(
          new Exception(
            s"The agument was not provided for uniary operation [$opName]"
          )
        )
    }
  }

}

class CExpressionImpl[+T](func: ExpressionRequest => Try[Option[T]])
    extends CExpression {
  def eval(request: ExpressionRequest): Try[Option[T]] = {
    val result = func(request)
    result
  }
}
