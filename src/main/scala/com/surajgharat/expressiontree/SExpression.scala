package com.surajgharat.expressiontree

object SExpType extends Enumeration {
    type SExpType = Value
    val Constant, Variable, Operation = Value
}

object SExpResultType extends Enumeration {
    type SExpResultType = Value
    val Number, Text, Bool, Datetime = Value
}

object SExpOperationType extends Enumeration {
    type SExpOperationType = Value
}

class SExpression(value: Any) {

}

object SExpression {
    def constant(value:Float) : SExpression = ???
}