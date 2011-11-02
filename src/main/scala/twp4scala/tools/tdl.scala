package twp4scala.tools

import scala.util.parsing.combinator.syntactical._

object TDL extends StandardTokenParsers with RegexParsers {

  lexical.delimiters ++= List("{", "}", ";", "=")
  lexical.reserved += ("int", "string", "binary", "any", "defined", "by", "struct",
    "optional", "sequence", "union", "case", "typedef", "message", "protocol", "ID")

  def identifier = ident
  def number = regex("""\d+""", error = "not a number")

  def `type` = primitiveType | identifier | "any" ~ "defined" ~ "by" ~ identifier
  def primitiveType = "int" | "string" | "binary" | "any"

  def typedef = structdef | sequencedef | uniondef | forwarddef
  def structdef = "struct" ~ identifier ~ opt("=" ~ "ID" ~ number) ~ "{" ~ rep1(field) ~ "}"
  def field = opt("optional") ~ `type` ~ identifier ~ ";"
  def sequencedef = "sequence" ~ "<" ~ `type` ~ ">" ~ identifier ~ ";"
  def uniondef = "union" ~ identifier ~ "{" ~ rep1(casedef) ~ "}"
  def casedef = "case" ~ number ~ ":" ~ `type` ~ identifier ~ ";"
  def forwarddef = "typedef" ~ identifier ~ ";"

  def messagedef = "message" ~ identifier ~ "=" ~ (messageid | "ID" ~ number) ~ "{" ~ rep(field) ~ "}"
  def messageid = """[0-7]""".r

  def protocol = "protocol" ~ identifier ~ "=" ~ "ID" ~ number ~ "{" ~ rep(protocolelement) ~ "}"
  def protocolelement = typedef | messagedef

  def specification = rep(protocol | messagedef | structdef)

  def parseAll[T](p: Parser[T], in: String): ParseResult[T] = phrase(p)(new lexical.Scanner(in))
  def parseFile[T](p: Parser[T], file: String): ParseResult[T] = parseAll(p, io.Source.fromFile(file).mkString)
}