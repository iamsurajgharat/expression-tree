package io.github.iamsurajgharat.expressiontree
package expressiontree

trait Record {
  def getValue(path: String): Option[RValue]
  def get[T <: RValue](path: String): Option[T]
  def put[T <: RValue](path: String, value: T): Record
  def remove(path: String): Record
  def getKeys() : Set[String]
}

object DataType extends Enumeration {
  type DataType = Value
  val AnyVal, Number, Text, Bool, Date, Datetime, Null = Value
}

class RecordImpl(private val data: Map[String, RValue]) extends Record {
  def this() = this(Map.empty[String, RValue])
  def getValue(path: String): Option[RValue] = data.get(path)
  def get[T <: RValue](path: String): Option[T] = data.get(path).map(_.asInstanceOf[T])
  def put[T <: RValue](path: String, value: T): Record = new RecordImpl(data + (path -> value))
  def remove(path: String): Record = new RecordImpl(data - path)
  def getKeys() : Set[String] = data.keySet
}

import DataType._
case class RecordSchema(schema:Map[String,DataType])

case class ExpressionContext(flag1: Boolean)

case class ExpressionRequest(record: Record, context: ExpressionContext)

sealed trait RValue {
  type T
  def get():T
}

case class RNumber(data:Float) extends RValue {
  type T = Float
  def get():T = data
}

object RNumber {
  implicit def intToNumber(data:Int):RNumber = RNumber(data)
  implicit def longToNumber(data:Long):RNumber = RNumber(data)
  implicit def floatToNumber(data:Float):RNumber = RNumber(data)
  implicit def doubleToNumber(data:Double):RNumber = RNumber(data.asInstanceOf[Float])
}

case class RText(data:String) extends RValue {
  type T = String
  def get():T = data
}

object RText {
  implicit def stringToText(data:String):RText = RText(data)
}

case class RBool(data:Boolean) extends RValue {
  type T = Boolean
  def get():T = data
}

object RBool {
  implicit def booleanToBool(data:Boolean):RBool = RBool(data)
}

case class RDate(data:org.joda.time.LocalDate) extends RValue {
  type T = org.joda.time.LocalDate
  def get():T = data
}

object RDate {
  implicit def localDateToRDate(data:org.joda.time.LocalDate):RDate = RDate(data)
}

case class RDateTime(data:org.joda.time.DateTime) extends RValue {
  type T = org.joda.time.DateTime
  def get():T = data
}

object RDateTime {
  implicit def dateTimeToRDateTime(data:org.joda.time.DateTime):RDateTime = RDateTime(data)
}