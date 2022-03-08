package ktedon.dumbledore

import cats.parse.Rfc5234.*
import cats.parse.{Parser as P1, Parser0 as P0}
import cats.parse.*

object TokenParsers:

  val identifier: P1[String] = (alpha ~ (alpha | digit).rep0).string

  val integer: P1[String] = digit.rep.string
  val float: P1[String]   = (digit.rep ~ P1.char('.') ~ digit.rep).string
  val boolean: P1[String] = (P1.string("#f") | P1.string("#t")).string
  val atom: P1[String]    = (P1.char(':') ~ identifier).string
  val string: P1[String]  = ( dquote ~ (
      P1.string("\\\"") |
      P1.string("\\\\") |
      (P1.string("\\u") | integer) |
      wsp |
      P1.charIn("!#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[]^_`abcdefghijklmnopqrstuvwxyz{|}~\n")
    ).rep0 ~
    dquote
  ).string

  // val comment: P1[String] =

  val `type` = P1.recursive[String](rec =>
    identifier |
    (P1.char('(') ~ identifier ~ (wsp | lf).rep ~ (rec | (wsp | lf).rep).rep0 ~ P1.char(')')).string)

  val accumulate: P1[String] = (
    (P1.string("accum") ~ (wsp | lf).rep ~ identifier ~
    (P1.char('.') ~ (identifier | P1.char('_'))).rep0).string
  )

  val `package`: P1[String] = (
    (P1.string("pkg") ~ wsp.rep ~ identifier ~
    (P1.char('.') ~ identifier).rep0).string
  ).string

  val namespace = P1.recursive[String](rec =>
    ((P1.char('/').rep | P1.char('\\').rep) ~
    (wsp | lf).rep ~ P1.string("space") ~ (wsp | lf).rep ~ identifier ~ (wsp | lf).rep ~ rec.rep0).string
  )

  val fileHeader: P1[String] = (
    `package` ~ lf ~ wsp.rep0 ~
    (accumulate ~ lf).rep0 ~ wsp.rep0
  ).string
