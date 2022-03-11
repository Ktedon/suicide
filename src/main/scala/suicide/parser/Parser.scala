package ktedon.suicide

import cats.parse.Rfc5234.*
import cats.parse.{Parser as P1, Parser0 as P0}
import cats.parse.*

object TokenParsers:
  val identifierChar: P1[String] = (alpha | P1.charIn("+-*/<>~,;=!@$^&")).string
  val identifier: P1[Identifier] =
    (identifierChar ~ (identifierChar | digit).rep0).map(v =>
      Identifier(v.toString)
    )
  val expressionVar: P1[ExpressionVar] =
    ((P1.char('%') ~ identifier).backtrack | P1.char('%')).map(v =>
      ExpressionVar(v.toString.tail)
    )
  val stringVar: P1[StringVar] =
    ((P1.char('\'') ~ identifier).backtrack | P1.char('\'')).map(v =>
      StringVar(v.toString.tail)
    )

  val integer: P1[IntegerVal] =
    ((P1.char('-') ~ digit.rep) | digit.rep).map(v => IntegerVal(v.toString))
  val naturalNum: P1[NaturalNum] = (digit.rep).map(v => NaturalNum(v.toString))
  val float: P1[FloatVal] =
    ((integer <* P1.char('.')) ~ naturalNum).map(v => FloatVal(v._1, v._2))
  val boolean: P1[BooleanVal] = (P1.string("#f") | P1.string("#t")).map {
    _.toString match
      case "#f" => BooleanVal(false)
      case _    => BooleanVal(true)
  }
  val atom: P1[AtomVal] =
    (P1.char(':') *> identifier).map(v => AtomVal(v.toString))
  val string: P1[StringVal] = (dquote *> (
    P1.string("\\\"") |
      P1.string("\\\\") |
      (P1.string("\\u") | integer) |
      wsp |
      P1.charIn(
        "!#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[]^_`abcdefghijklmnopqrstuvwxyz{|}~\n"
      )
  ).rep0 <*
    dquote).map(v => StringVal(v.toString))

  val nln: P1[String] = (crlf | lf | cr).string
  val nlnws: P1[String] = (crlf | lf | cr | wsp).string
  val comment: P1[String] = (P1.char('#') ~ P1.until0(nln)).string

  val accumulate: P1[Accumulate] = (
    ((P1.string("accum") ~ nlnws.rep) *> (identifier | P1.char('_'))
      .repSep(P1.char('.'))).map(v => Accumulate(v.map(_.toString)))
  )

  val `package`: P1[Package] = (
    ((P1.string("pkg") ~ wsp.rep) *> (identifier).repSep(P1.char('.'))).string
  ).map(v => Package(v.map(_.toString)))

  val kind: P1[KindVal] = (
    identifier.between((P1.string("kind") ~ wsp.rep), wsp.rep) ~
      P1.recursive[Kind](rec =>
        ((identifier.repSep(P1.char('.')) | rec)
          .repSep(
            nlnws.rep ~ P1.string("->") ~ nlnws.rep
          ))
          .between(P1.char('(') ~ nlnws.rep0, nlnws.rep0 ~ P1.char(')')).map (v =>
            KindNode(v)
          )
      ).map(v => KindNode(v))
  ).map(v => KindVal(v._1, v))

  val `type`: P1[String] = (
    P1.string("type") <* wsp.rep *> identifier <* wsp.rep *>
      P1.recursive[String](rec =>
        (P1.char('(') <* nlnws.rep0 *> (identifier
          .repSep(P1.char('.'))
          .repSep(nlnws.rep) | rec).repSep(
          nlnws.rep *> P1.string("->") <* nlnws.rep
        ) <* nlnws.rep0 *> P1.char(')')).string
      )
  ).string

  val operator: P1[String] = (
    (
      P1.string("prefix").backtrack |
        P1.string("prefixr").backtrack |
        P1.string("postfixl").backtrack |
        P1.string("postfix").backtrack |
        P1.string("infixl").backtrack |
        P1.string("infixr").backtrack |
        P1.string("infix").backtrack
    ) <* nlnws.rep0 *> identifier <* nlnws.rep0 *> naturalNum
  ).string

  val expression: P1[String] = (
    P1.recursive[String](rec =>
      ((P1.char('[') ~ rec.rep0.surroundedBy(nlnws.rep0) ~ P1.char(']')) |
        (P1.char('{') ~ rec.rep0.surroundedBy(nlnws.rep0) ~ P1.char('}')) |
        (P1.char('<') ~ rec.rep0.surroundedBy(nlnws.rep0) ~ P1.char('>')) |
        (P1.char('(') <* nlnws.rep0 ~ rec.rep0.surroundedBy(nlnws.rep0) ~ P1
          .char(')')) |
        float.backtrack | integer | comment | boolean | atom | string | identifier | nlnws.rep | expressionVar | stringVar | P1
          .char('_')).string
    ).rep
      .between(
        P1.char('('),
        P1.char(')')
      )
      .backtrack | P1.string("()")
  ).string

  val conditions: P1[String] = (
    P1.recursive[String](rec =>
      ((P1.char('[') ~ rec.rep0.surroundedBy(nlnws.rep0) ~ P1.char(']')) |
        (P1.char('{') ~ rec.rep0.surroundedBy(nlnws.rep0) ~ P1.char('}')) |
        (P1.char('<') ~ rec.rep0.surroundedBy(nlnws.rep0) ~ P1.char('>')) |
        (P1.char('(') <* nlnws.rep0 ~ rec.rep0.surroundedBy(nlnws.rep0) ~ P1
          .char(')')) |
        float.backtrack | integer | comment | boolean | atom | string | identifier | nlnws.rep | expressionVar | stringVar | P1
          .char('_')).string
    ).rep
      .repSep(
        nlnws.rep0 ~ (P1.string("and") | P1.string("or") | P1.string("xor") | P1
          .string("nand")) ~ nlnws.rep0
      )
      .between(
        P1.char('('),
        P1.char(')')
      )
      .backtrack | P1.string("()")
  ).string

  val simplePredicate: P1[String] = (
    identifier ~ nlnws.rep ~ expression
  ).string

  val complexPredicate: P1[String] = (
    simplePredicate ~ nlnws.rep0 ~ P1.string(":-") ~ nlnws.rep0 ~
      conditions
  ).string

  val moduleContent: P0[String] = (
    (kind.backtrack | `type`.backtrack | operator.backtrack | complexPredicate.backtrack | simplePredicate.backtrack | comment)
      .repSep0(nlnws.rep)
    )
    .string

  val module = P1.recursive[String](rec =>
    ((P1.char('/').rep | P1.char('\\').rep) ~
      nlnws.rep ~ P1.string("mod") <* nlnws.rep *>
      identifier ~ (moduleContent.backtrack | nlnws.rep | rec)).string
  )

  val namespaceContent: P1[String] = (
    (kind.backtrack | `type`.backtrack | operator.backtrack | module.backtrack | complexPredicate.backtrack | simplePredicate.backtrack | comment)
      .repSep(nlnws.rep)
    )
    .string

  val namespace = P1.recursive[String](rec =>
    ((P1.char('/').rep | P1.char('\\').rep) ~
      (nlnws.rep ~ P1.string("space") ~ nlnws.rep) ~
      identifier ~ (namespaceContent.backtrack | nlnws.rep | rec).rep).string
  )

  val fileHeader: P1[String] = (
    `package` ~ nln ~ nlnws.rep0 ~
      (accumulate | nln).rep0 ~ wsp.rep0
  ).string

  val fileParts: P1[String] = (
    fileHeader ~ namespace
  ).string
