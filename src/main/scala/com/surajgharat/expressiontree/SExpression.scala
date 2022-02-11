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
    etype: SExpType,
    rtype: DataType,
    value: Option[Any],
    path: Option[String],
    otype: Option[SExpOpType],
    args: Array[SExpression]
) {
  def this(value: Any, rtype: DataType) = {
    this(Constant, rtype, Some(value), None, None, Array.empty[SExpression])
  }

  def this(opType: SExpOpType, args: SExpression*) = {
    this(Operation, opType.resultType, None, None, Some(opType), args.toArray)
  }

  def compile(schema:RecordSchema): CExpression = {
    etype match {
      case SExpType.Constant  => compileConstant()
      case SExpType.Variable  => compileVariable(path.get, schema);
      case SExpType.Operation => compileOperation(schema)
    }
  }

  private def compileConstant(): CExpression = {
    rtype match {
      case DataType.Number =>
        new CExpression2[Float](_ => tryValue())
    }
  }

  private def compileVariable(key:String, schema:RecordSchema) : CExpression = {
    schema.schema.get(key) match {
        case Some(dt) => compileVariable(key, dt)
        case None => new CExpression2[Float](_ => Failure(new Exception(s"Undefined schema for key $key")))
    }
  }

  private def compileVariable(key:String, dt:DataType) : CExpression = {
    dt match {
        case DataType.Bool => new CExpression2[Boolean](req => Try(req.record.get[Boolean](key).get))
        case DataType.Number => new CExpression2[Float](req => Try(req.record.get[Float](key).get))
        case DataType.Text => new CExpression2[String](req => Try(req.record.get[String](key).get))
    }
  }

  private def compileOperation(schema:RecordSchema): CExpression = {
    otype.get match {
      case SExpOpType.AddOpr =>
        val e1 = args(0).compile(schema).asInstanceOf[CExpression2[Float]]
        val e2 = args(1).compile(schema).asInstanceOf[CExpression2[Float]]
        CExpression.addOpr2(e1, e2)

      case SExpOpType.SubtractOpr =>
        val e1 = args(0).compile(schema).asInstanceOf[CExpression2[Float]]
        val e2 = args(1).compile(schema).asInstanceOf[CExpression2[Float]]
        CExpression.subtractOpr(e1, e2)

      case SExpOpType.MultiplyOpr =>
        val e1 = args(0).compile(schema).asInstanceOf[CExpression2[Float]]
        val e2 = args(1).compile(schema).asInstanceOf[CExpression2[Float]]
        CExpression.multiplyOpr(e1, e2)

      case SExpOpType.DivideOpr =>
        val e1 = args(0).compile(schema).asInstanceOf[CExpression2[Float]]
        val e2 = args(1).compile(schema).asInstanceOf[CExpression2[Float]]
        CExpression.divideOpr(e1, e2)

      case SExpOpType.ModuleOpr =>
        val e1 = args(0).compile(schema).asInstanceOf[CExpression2[Float]]
        val e2 = args(1).compile(schema).asInstanceOf[CExpression2[Float]]
        CExpression.moduleOpr(e1, e2)

      case SExpOpType.GteOpr =>
        val e1 = args(0).compile(schema).asInstanceOf[CExpression2[Float]]
        val e2 = args(1).compile(schema).asInstanceOf[CExpression2[Float]]
        CExpression.gteOpr(e1, e2)

      case SExpOpType.GtOpr =>
        val e1 = args(0).compile(schema).asInstanceOf[CExpression2[Float]]
        val e2 = args(1).compile(schema).asInstanceOf[CExpression2[Float]]
        CExpression.gtOpr(e1, e2)

      case SExpOpType.LteOpr =>
        val e1 = args(0).compile(schema).asInstanceOf[CExpression2[Float]]
        val e2 = args(1).compile(schema).asInstanceOf[CExpression2[Float]]
        CExpression.lteOpr(e1, e2)

      case SExpOpType.LtOpr =>
        val e1 = args(0).compile(schema).asInstanceOf[CExpression2[Float]]
        val e2 = args(1).compile(schema).asInstanceOf[CExpression2[Float]]
        CExpression.ltOpr(e1, e2)

      case SExpOpType.EqOpr =>
        val e1 = args(0).compile(schema).asInstanceOf[CExpression2[Float]]
        val e2 = args(1).compile(schema).asInstanceOf[CExpression2[Float]]
        CExpression.eqOpr(e1, e2)

      case SExpOpType.NeqOpr =>
        val e1 = args(0).compile(schema).asInstanceOf[CExpression2[Float]]
        val e2 = args(1).compile(schema).asInstanceOf[CExpression2[Float]]
        CExpression.neqOpr(e1, e2)

      case SExpOpType.AndOpr =>
        val e1 = args(0).compile(schema).asInstanceOf[CExpression2[Boolean]]
        val e2 = args(1).compile(schema).asInstanceOf[CExpression2[Boolean]]
        CExpression.andOpr(e1, e2)

      case SExpOpType.OrOpr =>
        val e1 = args(0).compile(schema).asInstanceOf[CExpression2[Boolean]]
        val e2 = args(1).compile(schema).asInstanceOf[CExpression2[Boolean]]
        CExpression.orOpr(e1, e2)

    case SExpOpType.NegateOpr =>
        val e1 = args(0).compile(schema).asInstanceOf[CExpression2[Boolean]]
        CExpression.negateOpr(e1)
    }
  }

  private def tryValue(): Try[Float] =
    value.fold[Try[Float]](Failure(new Exception(s"Given value is null")))(
      toFloat _
    )

  private def toFloat(obj: Any): Try[Float] = obj match {
    case n1: Float => Success(n1)
    case _ => Failure(new Exception(s"Given value is not a number [$obj]"))
  }
}

object SExpression {
  def constant(value: Float): SExpression = new SExpression(value, Number)
  def constant(value: String): SExpression = new SExpression(value, Text)
  def constant(value: Boolean): SExpression = new SExpression(value, Bool)

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
