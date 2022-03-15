package ktedon.suicide

import cats.data.NonEmptyList

trait Token

case class Identifier(id: String) extends Token
case class Path(path: NonEmptyList[Identifier]) extends Token
case class ExpressionVar(name: String) extends Token
case class StringVar(name: String) extends Token
case class IntegerVal(num: String) extends Token
case class NaturalNum(num: String) extends Token
case class FloatVal(exponent: IntegerVal, mantissa: NaturalNum) extends Token
case class BooleanVal(num: Boolean) extends Token
case class AtomVal(num: String) extends Token
case class StringVal(num: String) extends Token

case class Whitespace(value: String) extends Token
case object Comment extends Token
case object Underscore extends Token

case class Accumulate(num: NonEmptyList[String]) extends Token
case class Package(num: IndexedSeq[String]) extends Token

trait Kind extends Token
case class KindLeaf(vals: Token) extends Kind
case class KindNode(vals: List[Token]) extends Kind
case class KindVal(identifier: Identifier, structure: KindNode) extends Kind

trait Type extends Token
case class TypeLeaf(vals: Token) extends Type
case class TypeNode(vals: List[Token]) extends Type
case class ConstructedType(path: NonEmptyList[Token]) extends Type
case class TypeVal(identifier: Identifier, structure: TypeNode) extends Type

trait OperatorFixity
case object Prefix extends OperatorFixity
case object Prefixr extends OperatorFixity
case object Postfixl extends OperatorFixity
case object Postfix extends OperatorFixity
case object Infix extends OperatorFixity
case object Infixr extends OperatorFixity
case object Infixl extends OperatorFixity

case class Operator(
    fixity: OperatorFixity,
    identifier: Identifier,
    precedence: NaturalNum
) extends Token

trait Expression extends Token
case class ExpressionLeaf(vals: Token) extends Expression
case class ExpressionNode(vals: List[Token]) extends Expression
case class ExpressionList(vals: List[Token]) extends Expression
case class ExpressionTuple(vals: List[Token]) extends Expression
case class ExpressionSet(vals: List[Token]) extends Expression
case class ExpressionVal(structure: ExpressionNode) extends Expression

trait ConditionOperator
case object And extends ConditionOperator
case object Or extends ConditionOperator
case object Xor extends ConditionOperator
case object Nand extends ConditionOperator

trait Condition extends Token
case class ConditionLeaf(vals: Token) extends Condition
case class ConditionNode(vals: List[Token]) extends Condition
case class ConditionList(vals: List[Token]) extends Condition
case class ConditionTuple(vals: List[Token]) extends Condition
case class ConditionSet(vals: List[Token]) extends Condition
case class ConditionVal(
    structure: Option[
      (NonEmptyList[Condition], List[(ConditionOperator, NonEmptyList[Condition])])
    ]
) extends Condition

trait Predicate extends Token
case class SimplePredicate(identifier: Identifier, expression: ExpressionVal) extends Predicate
case class ComplexPredicate(identifier: SimplePredicate, expression: ConditionVal) extends Predicate

trait Module extends Token
case class ModuleContent(tokens: NonEmptyList[Token]) extends Module
case class ModuleNode(scopeChange: String, identifier: Identifier, moduleContents: NonEmptyList[Token]) extends Module

trait Namespace extends Token
case class NamespaceContent(tokens: NonEmptyList[Token]) extends Namespace
case class NamespaceNode(scopeChange: String, identifier: Identifier, namespaceContents: NonEmptyList[Token]) extends Namespace
