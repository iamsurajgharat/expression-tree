package com.surajgharat.expressiontree

trait Record {
  def getValue(path: String): Option[Value]
  def get[T <: Value](path: String): Option[T]
  def put[T <: Value](path: String, value: T): Record
  def remove(path: String): Record
}

object DataType extends Enumeration {
  type DataType = Value
  val Number, Text, Bool, Datetime, Null = Value
}

class RecordImpl(private val data: Map[String, Value]) extends Record {
  def this() = this(Map.empty[String, Value])
  def getValue(path: String): Option[Value] = data.get(path)
  def get[T <: Value](path: String): Option[T] = data.get(path).map(_.asInstanceOf[T])
  def put[T <: Value](path: String, value: T): Record = new RecordImpl(data + (path -> value))
  def remove(path: String): Record = new RecordImpl(data - path)
}

import DataType._
case class RecordSchema(schema:Map[String,DataType])

case class ExpressionContext(flag1: Boolean)

case class ExpressionRequest(record: Record, context: ExpressionContext)

sealed trait Value {
  type T
  def get():T
}

case class Number(data:Float) extends Value {
  type T = Float
  def get():T = data
}

object Number {
  implicit def intToNumber(data:Int):Number = Number(data)
  implicit def longToNumber(data:Long):Number = Number(data)
  implicit def floatToNumber(data:Float):Number = Number(data)
  implicit def doubleToNumber(data:Double):Number = Number(data.asInstanceOf[Float])
}

case class Text(data:String) extends Value {
  type T = String
  def get():T = data
}

object Text {
  implicit def stringToText(data:String):Text = Text(data)
}

case class Bool(data:Boolean) extends Value {
  type T = Boolean
  def get():T = data
}

object Bool {
  implicit def booleanToBool(data:Boolean):Bool = Bool(data)
}
