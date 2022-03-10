package ktedon.suicide

import cats.parse.Rfc5234.*
import cats.parse.{Parser as P1, Parser0 as P0}
import cats.parse.*

object TokenParsers:
  val identifierChar: P1[String] = (alpha | P1.charIn("+-*/<>~,;=!@$^&")).string
  val identifier: P1[String] =
    (identifierChar ~ (identifierChar | digit).rep0).string
  val expressionVar: P1[String] = (P1.char('%')).string
  val stringVar: P1[String] = (P1.char('\'') ~ identifier).string

  val integer: P1[String] = ((P1.char('-') ~ digit.rep) | digit.rep).string
  val naturalNum: P1[String] = (digit.rep).string
  val float: P1[String] = (integer <* P1.char('.') *> digit.rep).string
  val boolean: P1[String] = (P1.string("#f") | P1.string("#t")).string
  val atom: P1[String] = (P1.char(':') *> identifier).string
  val string: P1[String] = (dquote *> (
    P1.string("\\\"") |
      P1.string("\\\\") |
      (P1.string("\\u") | integer) |
      wsp |
      P1.charIn(
        "!#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[]^_`abcdefghijklmnopqrstuvwxyz{|}~\n"
      )
  ).rep0 <*
    dquote).string

  val nln: P1[String] = (crlf | lf | cr).string
  val nlnws: P1[String] = (crlf | lf | cr | wsp).string

  val comment: P1[String] = (P1.char('#') ~ P1.until0(nln)).string

  val accumulate: P1[String] = (
    (P1.string("accum") <* nlnws.rep *> identifier ~
      (P1.char('.') *> (identifier | P1.char('_'))).rep0).string
  )

  val `package`: P1[String] = (
    (P1.string("pkg") <* wsp.rep *> identifier ~
      (P1.char('.') *> identifier).rep0).string
  ).string

  val kind: P1[String] = (
    P1.string("kind") <* wsp.rep *> identifier <* wsp.rep *>
      P1.recursive[String](rec =>
        (P1.char('(') <* nlnws.rep0 *> (identifier.repSep(P1.char('.')) | rec)
          .repSep(
            nlnws.rep *> P1.string("->") <* nlnws.rep
          ) <* nlnws.rep0 *> P1.char(')')).string
      )
  ).string

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
        float.backtrack | integer | comment | boolean | atom | string | identifier | nlnws.rep | expressionVar | stringVar | P1.char('_')).string
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
      identifier ~ (moduleContent
        .surroundedBy(nlnws.rep)
        .backtrack | nlnws.rep) ~ rec.rep0).string
  )

  val namespaceContent: P0[String] = (
    (kind.backtrack | `type`.backtrack | operator.backtrack | module.backtrack | complexPredicate.backtrack | simplePredicate.backtrack | comment)
      .repSep0(nlnws.rep)
    )
    .string

  val namespace = P1.recursive[String](rec =>
    ((P1.char('/').rep | P1.char('\\').rep) <*
      (nlnws.rep ~ P1.string("space") ~ nlnws.rep) *>
      identifier ~ (namespaceContent
        .surroundedBy(nlnws.rep)
        .backtrack | nlnws.rep) ~ rec.rep0).string
  )

  val fileHeader: P1[String] = (
    `package` ~ nln ~ nlnws.rep0 ~
      (accumulate | nln).rep0 ~ wsp.rep0
  ).string

  val fileParts: P1[String] = (
    fileHeader ~ namespace
  ).string
