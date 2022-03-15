package ktedon.suicide

import cats.parse.Rfc5234.*
import cats.parse.{Parser as P1, Parser0 as P0}
import cats.parse.*

object TokenParsers:
  val identifierChar: P1[String] = (alpha | P1.charIn("+-*/<>~,;=!@$^&|")).string
  val identifier: P1[Identifier] =
    (identifierChar ~ (identifierChar | digit).rep0).map(v =>
      Identifier(v.toString)
    )
  val path: P1[Path] = (identifier.repSep(P1.char('.'))).map(Path(_))
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

  val nln: P1[Whitespace] = (crlf | lf | cr).map(v => Whitespace(v.toString))
  val nlnws: P1[Whitespace] =
    (crlf | lf | cr | wsp).map(v => Whitespace(v.toString))
  val nlnwsr: P1[Whitespace] = (nlnws.rep).map(v => Whitespace(v.toString))
  val comment: P1[Token] = (P1.char('#') ~ P1.until0(nln)).map(v => Comment)
  val underscore: P1[Token] = P1.char('_').map(v => Underscore)


  val accumulate: P1[Accumulate] = (
    ((P1.string("accum") ~ nlnws.rep) *> (identifier | P1.char('_'))
      .repSep(P1.char('.'))).map(v => Accumulate(v.map(_.toString)))
  )

  val `package`: P1[Package] = (
    ((P1.string("pkg") ~ wsp.rep) *> (identifier).repSep(P1.char('.'))).string
  ).map(v => Package(v.map(_.toString)))

  // FIX: cases like "kind id ( hello -> ( why -> tho) )"
  val kind: P1[KindVal] = (
    identifier.between((P1.string("kind") ~ wsp.rep), wsp.rep) ~
      P1.recursive[KindNode](rec =>
        (
          path.map(KindLeaf(_)) | rec
        ).repSep(nlnwsr ~ P1.string("->") ~ nlnwsr)
          .between(P1.char('(') ~ nlnws.rep0, nlnws.rep0 ~ P1.char(')'))
          .map(v => KindNode(v.toList))
      )
  ).map(v => KindVal(v._1, v._2))

  // FIX: cases like "type id ( hello -> ( why -> tho) )"
  val `type`: P1[TypeVal] = (
    identifier.between((P1.string("type") ~ wsp.rep), wsp.rep) ~
    P1.recursive[TypeNode](rec =>
      (
        path.map(TypeLeaf(_)) | rec
      ).repSep(nlnwsr).map(ConstructedType(_)).repSep(nlnwsr ~ P1.string("->") ~ nlnwsr)
        .between(P1.char('(') ~ nlnws.rep0, nlnws.rep0 ~ P1.char(')'))
        .map(v => TypeNode(v.toList))
    )
  ).map(v => TypeVal(v._1, v._2))

  val operator: P1[Operator] = (
    (
      P1.string("prefix").backtrack |
        P1.string("prefixr").backtrack |
        P1.string("postfixl").backtrack |
        P1.string("postfix").backtrack |
        P1.string("infixl").backtrack |
        P1.string("infixr").backtrack |
        P1.string("infix").backtrack
    ).string ~ identifier.surroundedBy(nlnws.rep0) ~ naturalNum
  ).map { v =>
    val fixity = v._1._1 match
      case "prefix" => Prefix
      case "prefixr" => Prefixr
      case "postfixl" => Postfixl
      case "postfix" => Postfix
      case "infixl" => Infixl
      case "infixr" => Infixr
      case "infix" => Infix
    Operator(fixity, v._1._2, v._2)
  }

  val expression: P1[ExpressionVal] = (
    P1.recursive[Expression](rec =>
      ((P1.char('[') ~ rec.rep0.surroundedBy(nlnws.rep0) ~ P1.char(']')).map(v => ExpressionList(v._1._2)) |
        (P1.char('{') ~ rec.rep0.surroundedBy(nlnws.rep0) ~ P1.char('}')).map(v => ExpressionTuple(v._1._2)) |
        (P1.char('<') ~ rec.rep0.surroundedBy(nlnws.rep0) ~ P1.char('>')).map(v => ExpressionSet(v._1._2)) |
        (P1.char('(') ~ rec.rep0.surroundedBy(nlnws.rep0) ~ P1
          .char(')')).map(v => ExpressionNode(v._1._2)) |
        float.map(ExpressionLeaf(_)).backtrack | integer.map(ExpressionLeaf(_)) | comment.map(ExpressionLeaf(_)) | boolean.map(ExpressionLeaf(_)) | atom.map(ExpressionLeaf(_)) | string.map(ExpressionLeaf(_)) | identifier.map(ExpressionLeaf(_)) | nlnwsr.map(ExpressionLeaf(_)) | expressionVar.map(ExpressionLeaf(_)) | stringVar.map(ExpressionLeaf(_)) | underscore.map(ExpressionLeaf(_)))
    ).rep
      .between(
        P1.char('('),
        P1.char(')')
      ).map(v => ExpressionNode(v.toList))
      .backtrack | P1.string("()").map(v => ExpressionNode(List.empty))
  ).map(v => ExpressionVal(v))


  val conditions: P1[ConditionVal] = {
    val condition = P1.recursive[Condition](rec =>
      ((P1.char('[') ~ rec.rep0.surroundedBy(nlnws.rep0) ~ P1.char(']')).map(v => ConditionList(v._1._2)) |
        (P1.char('{') ~ rec.rep0.surroundedBy(nlnws.rep0) ~ P1.char('}')).map(v => ConditionTuple(v._1._2)) |
        (P1.char('<') ~ rec.rep0.surroundedBy(nlnws.rep0) ~ P1.char('>')).map(v => ConditionSet(v._1._2)) |
        (P1.char('(') ~ rec.rep0.surroundedBy(nlnws.rep0) ~ P1
          .char(')')).map(v => ConditionNode(v._1._2)) |
        float.map(ConditionLeaf(_)).backtrack | integer.map(ConditionLeaf(_)) | comment.map(ConditionLeaf(_)) | boolean.map(ConditionLeaf(_)) | atom.map(ConditionLeaf(_)) | string.map(ConditionLeaf(_)) | identifier.map(ConditionLeaf(_)) | nlnwsr.map(ConditionLeaf(_)) | expressionVar.map(ConditionLeaf(_)) | stringVar.map(ConditionLeaf(_)) | underscore.map(ConditionLeaf(_)))
    ).rep
    val conditionOperator = (P1.string("and") | P1.string("or") | P1.string("xor") | P1
      .string("nand")).surroundedBy(nlnws.rep0).string.map(
        _ match
          case "and" => And
          case "or" => Or
          case "xor" => Xor
          case "nand" => Nand
      )

    (condition ~ (conditionOperator ~ condition).rep0)
      .between(
        P1.char('('),
        P1.char(')')
      ).map(v => ConditionVal(Some(v)))
      .backtrack | P1.string("()").map(v => ConditionVal(None))
  }

  val simplePredicate: P1[SimplePredicate] = (
    identifier ~ nlnws.rep ~ expression
  ).map(v => SimplePredicate(v._1._1, v._2))

  val complexPredicate: P1[ComplexPredicate] = (
    simplePredicate ~ (nlnws.rep0 ~ P1.string(":-") ~ nlnws.rep0) ~
      conditions
  ).map(v => ComplexPredicate(v._1._1, v._2))

  val moduleContent: P1[ModuleContent] = (
    kind.backtrack | `type`.backtrack | operator.backtrack | complexPredicate.backtrack | simplePredicate.backtrack | comment
  ).repSep(nlnwsr).map(v => ModuleContent(v))

  val module = P1.recursive[ModuleNode](rec =>
    ((P1.char('/').rep | P1.char('\\').rep).string ~
      (nlnws.rep ~ P1.string("mod") <* nlnws.rep) ~
      identifier ~ (moduleContent.backtrack | nlnwsr | rec).rep).map(v => ModuleNode(v._1._1._1, v._1._2, v._2))
  )

  val namespaceContent: P1[NamespaceContent] = (
    kind.backtrack | `type`.backtrack | operator.backtrack | module.backtrack | complexPredicate.backtrack | simplePredicate.backtrack | comment
  ).repSep(nlnwsr).map(v => NamespaceContent(v))

  val namespace = P1.recursive[NamespaceNode](rec =>
    ((P1.char('/').rep | P1.char('\\').rep).string ~
      P1.string("space").surroundedBy(nlnwsr) ~
      identifier ~ (namespaceContent.backtrack | nlnwsr | rec).rep).map(v => NamespaceNode(v._1._1._1, v._1._2, v._2))
  )

  val fileHeader: P1[String] = (
    `package` ~ nln ~ nlnws.rep0 ~
      (accumulate | nln).rep0 ~ wsp.rep0
  ).string

  val fileParts: P1[String] = (
    fileHeader ~ namespace
  ).string
