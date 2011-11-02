package twp4scala.tools

import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator.{ImplicitConversions => Flatten}
import twp4scala.tools.ast._

object TDL extends StandardTokenParsers with RegexParsers with Flatten {

  lexical.delimiters ++= List("{", "}", ";", "=")
  lexical.reserved += ("int", "string", "binary", "any", "defined", "by", "struct",
    "optional", "sequence", "union", "case", "typedef", "message", "protocol", "ID")

  def identifier: Parser[Identifier] = ident ^^ (Identifier(_))
  def number: Parser[Int] = regex("""\d+""", error = "not a number") ^^ (_.toInt)

  def `type`: Parser[Type] = (
      primitiveType
    | identifier
    | "any" ~ "defined" ~ "by" ~> identifier ^^ (id => AnyDefinedBy(id))
  )
  def primitiveType: Parser[PrimitiveType] = (
      "int"     ^^ (_ => IntType)
    | "string"  ^^ (_ => StringType)
    | "binary"  ^^ (_ => BinaryType)
    | "any"     ^^ (_ => AnyType)
  )

  def typedef: Parser[TypeDefinition] = structdef | sequencedef | uniondef | forwarddef

  def structdef = "struct" ~> identifier ~ opt("=" ~ "ID" ~> number) ~ ("{" ~> rep1(field) <~ "}") ^^ (
    (id: Identifier, num: Option[Int], fields: List[Field]) => StructDefinition(id, num.map(_.toInt), fields)
  )
  def field = opt("optional") ~ `type` ~ identifier <~ ";" ^^ (
    (opt: Option[String], tp: Type, id: Identifier) => Field(opt.isDefined, tp, id)
  )
  def sequencedef = ("sequence" ~ "<" ~> `type` <~ ">") ~ identifier <~ ";" ^^ (
    (tp: Type, id: Identifier) => SequenceDefinition(tp, id)
  )
  def uniondef = ("union" ~> identifier <~ "{") ~ rep1(casedef) <~ "}" ^^ (
    (id: Identifier, cases: List[CaseDefinition]) => UnionDefinition(id, cases)
  )
  def casedef = ("case" ~> number <~ ":") ~ `type` ~ identifier <~ ";" ^^ (
    (num: Int, tp: Type, id: Identifier) => CaseDefinition(num, tp, id)
  )
  def forwarddef = "typedef" ~> identifier <~ ";" ^^ (
    (id: Identifier) => ForwardDefinition(id)
  )

  def messagedef = ("message" ~> identifier <~ "=") ~
      (   messageid       ^^ (_.toInt)
        | "ID" ~> number  ^^ (_.toInt)
      ) ~ ("{" ~> rep(field) <~ "}") ^^ (
    (id: Identifier, num: Int, fields: List[Field]) => MessageDefinition(id, num, fields)
  )
  def messageid = """[0-7]""".r

  def protocol = "protocol" ~> identifier ~ ("=" ~ "ID" ~> number <~ "{") ~ rep(protocolelement) <~ "}" ^^ (
    (id: Identifier, num: Int, elements: List[ProtocolElement]) => Protocol(id, num, elements)
  )
  def protocolelement: Parser[ProtocolElement] = typedef | messagedef

  def specification = rep(protocol | messagedef | structdef) ^^ (
    (elements: List[SpecificationElement]) => Specification(elements)
  )

  def parseAll[T](p: Parser[T], in: String): ParseResult[T] = phrase(p)(new lexical.Scanner(in))
  def parseFile[T](p: Parser[T], file: String): ParseResult[T] = parseAll(p, io.Source.fromFile(file).mkString)
}
