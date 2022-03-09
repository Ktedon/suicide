package ktedon.suicide

import cats.parse.Rfc5234.*
import cats.parse.{Parser as P1, Parser0 as P0}
import cats.parse.*

object TokenParsers:

  val identifier: P1[String] = (alpha ~ (alpha | digit).rep0).string

  val integer: P1[String] = digit.rep.string
  val float: P1[String] = (digit.rep ~ P1.char('.') ~ digit.rep).string
  val boolean: P1[String] = (P1.string("#f") | P1.string("#t")).string
  val atom: P1[String] = (P1.char(':') ~ identifier).string
  val string: P1[String] = (dquote ~ (
    P1.string("\\\"") |
      P1.string("\\\\") |
      (P1.string("\\u") | integer) |
      wsp |
      P1.charIn(
        "!#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[]^_`abcdefghijklmnopqrstuvwxyz{|}~\n"
      )
  ).rep0 ~
    dquote).string

  val nln: P1[String] = (crlf | lf | cr).string
  val nlnws: P1[String] = (crlf | lf | cr | wsp).string

  // val comment: P1[String] =

  val accumulate: P1[String] = (
    (P1.string("accum") ~ (wsp | nln).rep ~ identifier ~
      (P1.char('.') ~ (identifier | P1.char('_'))).rep0).string
  )

  val `package`: P1[String] = (
    (P1.string("pkg") ~ wsp.rep ~ identifier ~
      (P1.char('.') ~ identifier).rep0).string
  ).string

  val kind: P1[String] = (
    P1.string("kind") ~ wsp.rep ~ identifier ~ wsp.rep ~
      P1.recursive[String](rec =>
        (P1.char('(') ~ nlnws.rep0 ~ (identifier.repSep(P1.char('.')) | rec)
          .repSep(
            nlnws.rep ~ P1.string("->") ~ nlnws.rep
          ) ~ nlnws.rep0 ~ P1.char(')')).string
      )
  ).string

  val `type`: P1[String] = (
    P1.string("type") ~ wsp.rep ~ identifier ~ wsp.rep ~
      P1.recursive[String](rec =>
        (P1.char('(') ~ nlnws.rep0 ~ (identifier
          .repSep(P1.char('.'))
          .repSep(nlnws.rep) | rec).repSep(
          nlnws.rep ~ P1.string("->") ~ nlnws.rep
        ) ~ nlnws.rep0 ~ P1.char(')')).string
      )
  ).string

  val namespaceContent: P0[String] = (
    (kind | `type`).repSep0(nlnws.rep)
  ).string

  val namespace = P1.recursive[String](rec =>
    ((P1.char('/').rep | P1.char('\\').rep) ~
      nlnws.rep ~ P1.string("space") ~ nlnws.rep ~
      identifier ~ nlnws.rep ~ namespaceContent.? ~ nlnws.rep ~ rec.rep0).string
  )

  val fileHeader: P1[String] = (
    `package` ~ nln ~ (wsp | nln).rep0 ~
      (accumulate | nln).rep0 ~ wsp.rep0
  ).string

  val fileParts: P1[String] = (
    fileHeader ~ namespace
  ).string
