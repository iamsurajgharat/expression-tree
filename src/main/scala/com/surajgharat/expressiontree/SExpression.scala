package com.surajgharat.expressiontree

import scala.util.Try
import scala.util.Success
import scala.util.Failure

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
      case DataType.Null =>
        CExpression.constant(null:Any)
    }
  }

  private def compileVariable(key: String, dt: DataType): CExpression = {
    dt match {
      case DataType.Bool =>
        new CExpressionImpl[Boolean](req =>
            Try(req.record.get[Bool](key).map(_.get().asInstanceOf[Boolean]))
        )
      case DataType.Number => new CExpressionImpl[Float](req => Try(req.record.get[Number](key).map(_.get().asInstanceOf[Float])))
      case DataType.Text => new CExpressionImpl[String](req => Try(req.record.get[Text](key).map(_.get().asInstanceOf[String])))
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
    }
  }

  private def tryValue[T](): Try[Option[T]] =
    value.fold[Try[Option[T]]](Failure(new Exception(s"Given value is null")))(
      toType[T](_)
    )

  private def toType[T](obj: Any): Try[Option[T]] = obj match {
    case n1: T => Success(Some(n1))
    case _     => Failure(new Exception(s"Given value is not of expected type [$obj]"))
  }
}

object SExpression {
  def constant(value: Float): SExpression = new SExpression(value, Number)
  def constant(value: String): SExpression = {
    if(value == null || value.trim() == "") constantNull
    else new SExpression(value, Text)
  }
  def constant(value: Boolean): SExpression = new SExpression(value, Bool)
  val constantNull : SExpression = new SExpression(null, DataType.Null)

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
}
