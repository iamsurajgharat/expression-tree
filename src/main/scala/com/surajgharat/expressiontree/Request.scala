package com.surajgharat.expressiontree

trait Record {
  def getAny(path: String): Option[Any]
  def get[T](path: String): Option[T]
  def put[T](path: String, value: T): Record
  def remove(path: String): Record
}

object DataType extends Enumeration {
  type DataType = Value
  val Number, Text, Bool, Datetime = Value
}

class RecordImpl(private val data: Map[String, Any]) extends Record {
  def this() = this(Map.empty[String, Any])
  def getAny(path: String): Option[Any] = data.get(path)
  def get[T](path: String): Option[T] = data.get(path).map(_.asInstanceOf[T])
  def put[T](path: String, value: T): Record = new RecordImpl(data + (path -> value))
  def remove(path: String): Record = new RecordImpl(data - path)
}

import DataType._
case class RecordSchema(schema:Map[String,DataType])

case class ExpressionContext(flag1: Boolean)

case class ExpressionRequest(record: Record, context: ExpressionContext)
