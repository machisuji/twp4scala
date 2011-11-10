package twp4scala.tools

import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator.{ImplicitConversions => Flatten}
import twp4scala.tools.ast._

object TDL extends StandardTokenParsers with RegexParsers with Flatten {

  lexical.delimiters ++= List("{", "}", ";", "=", "<", ">")
  lexical.reserved += ("int", "string", "binary", "any", "defined", "by", "struct",
    "optional", "sequence", "union", "case", "typedef", "message", "protocol", "ID")

  def identifier = ident ^^ (Identifier)
  def number: Parser[Int] = regex("""\d+""", error = "not a number") ^^ (_.toInt)

  def `type`: Parser[Type] = (
      "any" ~ "defined" ~ "by" ~> identifier ^^ (AnyDefinedBy)
    | identifier
    | primitiveType
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
  def sequencedef = ("sequence" ~ "<" ~> `type` <~ ">") ~ identifier <~ ";" ^^ (SequenceDefinition)
  def uniondef = ("union" ~> identifier <~ "{") ~ rep1(casedef) <~ "}" ^^ (UnionDefinition)
  def casedef = ("case" ~> number <~ ":") ~ `type` ~ identifier <~ ";" ^^ (CaseDefinition)
  def forwarddef = "typedef" ~> identifier <~ ";" ^^ (ForwardDefinition)

  def messagedef = ("message" ~> identifier <~ "=") ~
      (   messageid       ^^ (_.toInt)
        | "ID" ~> number  ^^ (_.toInt)
      ) ~ ("{" ~> rep(field) <~ "}") ^^ (MessageDefinition)
  def messageid = """[0-7]""".r

  def protocol = "protocol" ~> identifier ~ ("=" ~ "ID" ~> number <~ "{") ~ rep(protocolelement) <~ "}" ^^ (Protocol)
  def protocolelement: Parser[ProtocolElement] = typedef | messagedef

  def specification = rep(protocol | messagedef | structdef) ^^ (Specification)

  def parseAll[T](p: Parser[T], in: String): ParseResult[T] = phrase(p)(new lexical.Scanner(in))
  def parseFile[T](p: Parser[T], file: String): ParseResult[T] = parseAll(p, io.Source.fromFile(file).mkString)
}
