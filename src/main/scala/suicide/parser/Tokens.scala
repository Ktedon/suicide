package ktedon.suicide

import cats.data.NonEmptyList

trait Token

case class Identifier(id: String) extends Token
case class ExpressionVar(name: String) extends Token
case class StringVar(name: String) extends Token
case class IntegerVal(num: String) extends Token
case class NaturalNum(num: String) extends Token
case class FloatVal(exponent: IntegerVal, mantissa: NaturalNum) extends Token
case class BooleanVal(num: Boolean) extends Token
case class AtomVal(num: String) extends Token
case class StringVal(num: String) extends Token

case class Accumulate(num: NonEmptyList[String]) extends Token
case class Package(num: IndexedSeq[String]) extends Token

trait Kind extends Token
case class KindLeaf(vals: Identifier) extends Kind
case class KindNode(vals: List[Kind]) extends Kind
case class KindVal(identifier: Identifier, structure: KindNode)
