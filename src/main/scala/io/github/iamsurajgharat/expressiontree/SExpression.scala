package io.github.iamsurajgharat
package expressiontree

import scala.util.Try
import scala.util.Success
import scala.util.Failure
import io.github.iamsurajgharat.expressiontree.expressiontree._

object SExpType extends Enumeration {
  type SExpType = Value
  val Constant, Variable, Operation = Value
}

object SExpOpType extends Enumeration {
  import DataType._
  protected case class OpTypeValue(
      isFunc: Boolean,
      numberOfRequiredArgs: Int,
      resultType: DataType
  ) extends super.Val {}

  type SExpOpType = OpTypeValue

  // arithmatic
  val AddOpr = OpTypeValue(false, 2, DataType.Number)
  val SubtractOpr = OpTypeValue(false, 2, DataType.Number)
  val MultiplyOpr = OpTypeValue(false, 2, DataType.Number)
  val DivideOpr = OpTypeValue(false, 2, DataType.Number)
  val ModuleOpr = OpTypeValue(false, 2, DataType.Number)

  // comparison
  val GteOpr = OpTypeValue(false, 2, DataType.Bool)
  val GtOpr = OpTypeValue(false, 2, DataType.Bool)
  val LteOpr = OpTypeValue(false, 2, DataType.Bool)
  val LtOpr = OpTypeValue(false, 2, DataType.Bool)

  // equality
  val EqOpr = OpTypeValue(false, 2, DataType.Bool)
  val NeqOpr = OpTypeValue(false, 2, DataType.Bool)

  // logical
  val AndOpr = OpTypeValue(false, 2, DataType.Bool)
  val OrOpr = OpTypeValue(false, 2, DataType.Bool)
  val NegateOpr = OpTypeValue(false, 1, DataType.Bool)

  // functions
  /*
    STARTSWITH
    ENDSWITH
    CONTAINS
    IF()
   */

  val StartsWithFun = OpTypeValue(true, 2, DataType.Bool)
  val IfFun = OpTypeValue(true, 3, DataType.AnyVal)
  val DayFun = OpTypeValue(true, 1, DataType.Number)
  val HourFun = OpTypeValue(true, 1, DataType.Number)
  val IsBlankFun = OpTypeValue(true, 1, DataType.Bool)
}

import SExpType._
import DataType._
import SExpOpType._

class SExpression(
    val etype: SExpType,
    val rtype: DataType,
    val value: Option[Any],
    val path: Option[String],
    val otype: Option[SExpOpType],
    val args: Array[SExpression]
) {
  def this(value: Any, rtype: DataType) = {
    this(Constant, rtype, Some(value), None, None, Array.empty[SExpression])
  }

  def this(opType: SExpOpType, args: SExpression*) = {
    this(Operation, opType.resultType, None, None, Some(opType), args.toArray)
  }

  def compile(): CExpression = {
    etype match {
      case SExpType.Constant  => compileConstant()
      case SExpType.Variable  => compileVariable(path.get, rtype);
      case SExpType.Operation => compileOperation()
    }
  }

  private def compileConstant(): CExpression = {
    rtype match {
      case DataType.Number =>
        new CExpressionImpl[Float](_ => tryValue())
      case DataType.Bool =>
        new CExpressionImpl[Boolean](_ => tryValue())
      case DataType.Text =>
        new CExpressionImpl[String](_ => tryValue())
      case DataType.Date =>
        new CExpressionImpl[org.joda.time.LocalDate](_ => tryValue())
      case DataType.Datetime =>
        new CExpressionImpl[org.joda.time.DateTime](_ => tryValue())
      case DataType.Null =>
        CExpression.constant(null: Any)
    }
  }

  private def compileVariable(key: String, dt: DataType): CExpression = {
    dt match {
      case DataType.Bool =>
        new CExpressionImpl[Boolean](req =>
          Try(req.record.get[RBool](key).map(_.get().asInstanceOf[Boolean]))
        )
      case DataType.Number =>
        new CExpressionImpl[Float](req =>
          Try(req.record.get[RNumber](key).map(_.get().asInstanceOf[Float]))
        )
      case DataType.Text =>
        new CExpressionImpl[String](req =>
          Try(req.record.get[RText](key).map(_.get().asInstanceOf[String]))
        )
      case DataType.Date =>
        new CExpressionImpl[org.joda.time.LocalDate](req =>
          Try(req.record.get[RDate](key).map(_.get().asInstanceOf[org.joda.time.LocalDate]))
        )
      case DataType.Datetime =>
        new CExpressionImpl[org.joda.time.DateTime](req =>
          Try(req.record.get[RDateTime](key).map(_.get().asInstanceOf[org.joda.time.DateTime]))
        )
    }
  }

  private def compileOperation(): CExpression = {
    otype.get match {
      case SExpOpType.AddOpr =>
        val e1 = args(0).compile().asInstanceOf[CExpressionImpl[Float]]
        val e2 = args(1).compile().asInstanceOf[CExpressionImpl[Float]]
        CExpression.addOpr(e1, e2)

      case SExpOpType.SubtractOpr =>
        val e1 = args(0).compile().asInstanceOf[CExpressionImpl[Float]]
        val e2 = args(1).compile().asInstanceOf[CExpressionImpl[Float]]
        CExpression.subtractOpr(e1, e2)

      case SExpOpType.MultiplyOpr =>
        val e1 = args(0).compile().asInstanceOf[CExpressionImpl[Float]]
        val e2 = args(1).compile().asInstanceOf[CExpressionImpl[Float]]
        CExpression.multiplyOpr(e1, e2)

      case SExpOpType.DivideOpr =>
        val e1 = args(0).compile().asInstanceOf[CExpressionImpl[Float]]
        val e2 = args(1).compile().asInstanceOf[CExpressionImpl[Float]]
        CExpression.divideOpr(e1, e2)

      case SExpOpType.ModuleOpr =>
        val e1 = args(0).compile().asInstanceOf[CExpressionImpl[Float]]
        val e2 = args(1).compile().asInstanceOf[CExpressionImpl[Float]]
        CExpression.moduleOpr(e1, e2)

      case SExpOpType.GteOpr =>
        val e1 = args(0).compile().asInstanceOf[CExpressionImpl[Float]]
        val e2 = args(1).compile().asInstanceOf[CExpressionImpl[Float]]
        CExpression.gteOpr(e1, e2)

      case SExpOpType.GtOpr =>
        val e1 = args(0).compile().asInstanceOf[CExpressionImpl[Float]]
        val e2 = args(1).compile().asInstanceOf[CExpressionImpl[Float]]
        CExpression.gtOpr(e1, e2)

      case SExpOpType.LteOpr =>
        val e1 = args(0).compile().asInstanceOf[CExpressionImpl[Float]]
        val e2 = args(1).compile().asInstanceOf[CExpressionImpl[Float]]
        CExpression.lteOpr(e1, e2)

      case SExpOpType.LtOpr =>
        val e1 = args(0).compile().asInstanceOf[CExpressionImpl[Float]]
        val e2 = args(1).compile().asInstanceOf[CExpressionImpl[Float]]
        CExpression.ltOpr(e1, e2)

      case SExpOpType.EqOpr =>
        val e1 = args(0).compile().asInstanceOf[CExpressionImpl[Any]]
        val e2 = args(1).compile().asInstanceOf[CExpressionImpl[Any]]
        CExpression.eqOpr(e1, e2)

      case SExpOpType.NeqOpr =>
        val e1 = args(0).compile().asInstanceOf[CExpressionImpl[Any]]
        val e2 = args(1).compile().asInstanceOf[CExpressionImpl[Any]]
        CExpression.neqOpr(e1, e2)

      case SExpOpType.AndOpr =>
        val e1 = args(0).compile().asInstanceOf[CExpressionImpl[Boolean]]
        val e2 = args(1).compile().asInstanceOf[CExpressionImpl[Boolean]]
        CExpression.andOpr(e1, e2)

      case SExpOpType.OrOpr =>
        val e1 = args(0).compile().asInstanceOf[CExpressionImpl[Boolean]]
        val e2 = args(1).compile().asInstanceOf[CExpressionImpl[Boolean]]
        CExpression.orOpr(e1, e2)

      case SExpOpType.NegateOpr =>
        val e1 = args(0).compile().asInstanceOf[CExpressionImpl[Boolean]]
        CExpression.negateOpr(e1)

      // functions
      // STARTSWITH
      case SExpOpType.StartsWithFun =>
        val e1 = args(0).compile().asInstanceOf[CExpressionImpl[String]]
        val e2 = args(1).compile().asInstanceOf[CExpressionImpl[String]]
        CExpression.createFuncStartsWith(e1, e2)

      // DAY
      case SExpOpType.DayFun =>
        val e1 = args(0).compile().asInstanceOf[CExpressionImpl[org.joda.time.LocalDate]]
        CExpression.createFuncDay(e1)
      
      // HOUR
      case SExpOpType.HourFun =>
        val e1 = args(0).compile().asInstanceOf[CExpressionImpl[org.joda.time.DateTime]]
        CExpression.createFuncHour(e1)

      // IF
      case SExpOpType.IfFun =>
        
        val e1 = args(0).compile().asInstanceOf[CExpressionImpl[Boolean]]

        def createFuncIf[T]() = {
          val e2 = args(1).compile().asInstanceOf[CExpressionImpl[T]]
          val e3 = args(2).compile().asInstanceOf[CExpressionImpl[T]]
          CExpression.createFuncIf(e1, e2, e3)
        }

        args(1).rtype match {
          case DataType.Bool => createFuncIf[Boolean]()
          case DataType.Date => createFuncIf[org.joda.time.LocalDate]()
          case DataType.Datetime => createFuncIf[org.joda.time.DateTime]()
          case DataType.Number => createFuncIf[Float]()
          case DataType.Text => createFuncIf[String]()
        }
    }
  }

  private def tryValue[T](): Try[Option[T]] =
    value.fold[Try[Option[T]]](Failure(new Exception(s"Given value is null")))(
      toType[T](_)
    )

  private def toType[T](obj: Any): Try[Option[T]] = obj match {
    case n1: T => Success(Some(n1))
    case _ =>
      Failure(new Exception(s"Given value is not of expected type [$obj]"))
  }
}

object SExpression {
  import DataType._
  def constant(value: Float): SExpression = new SExpression(value, Number)
  def constant(value: String): SExpression = {
    if (value == null || value.trim() == "") constantNull
    else new SExpression(value, Text)
  }
  def constant(value: Boolean): SExpression = new SExpression(value, Bool)
  def constant(value: org.joda.time.LocalDate): SExpression = new SExpression(value, Date)
  def constant(value: org.joda.time.DateTime): SExpression = new SExpression(value, Datetime)
  val constantNull: SExpression = new SExpression(null, DataType.Null)

  def variable(path: String, rtype: DataType): SExpression =
    new SExpression(
      Variable,
      rtype,
      None,
      Some(path),
      None,
      Array.empty[SExpression]
    )

  def operation(otype: SExpOpType, args: SExpression*): SExpression =
    new SExpression(otype, args: _*)

  def createFuncStartsWith(e1: SExpression, e2: SExpression): Try[SExpression] = {
    if (e1.rtype != DataType.Text || e2.rtype != DataType.Text)
      Failure(
        new ExpValidationException(
          "StartsWith : Data type of one or more arguments was/were incorrect"
        )
      )
    else
      Success(operation(SExpOpType.StartsWithFun, e1, e2))
  }

  def createFuncDay(e1: SExpression) : Try[SExpression] = {
    if (e1.rtype != DataType.Date && e1.rtype != DataType.Datetime)
      Failure(
        new ExpValidationException(
          "DAY : Data type of the argument was incorrect"
        )
      )
    else
      Success(operation(SExpOpType.DayFun, e1))
  }

  def createFuncHour(e1: SExpression) : Try[SExpression] = {
    if (e1.rtype != DataType.Datetime)
      Failure(
        new ExpValidationException(
          "HOUR : Data type of the argument was incorrect"
        )
      )
    else
      Success(operation(SExpOpType.HourFun, e1))
  }

  def createFuncIf(e1:SExpression, e2:SExpression, e3:SExpression):Try[SExpression] = {
    if(e1.rtype != DataType.Bool){
      Failure(
        new ExpValidationException(
          "IF : first argument must of type bool"
        )
      )
    } else if(e2.rtype != e3.rtype) {
      Failure(
        new ExpValidationException(
          "IF : second and third arguments must have same return type"
        )
      )
    } else {
      Success(operation(SExpOpType.IfFun, e1, e2, e3))
    }
  }

  def createFuncIsBlank(e1: SExpression) : Try[SExpression] = ???
}
