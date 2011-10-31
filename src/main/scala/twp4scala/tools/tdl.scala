package twp4scala.tools

import scala.util.parsing.combinator.syntactical._
import scala.util.matching.Regex
import java.io.Reader

object TDL extends StandardTokenParsers {

  lexical.delimiters ++= List("{", "}", ";", "=")
  lexical.reserved += ("int", "string", "binary", "any", "defined", "by", "struct",
    "optional", "sequence", "union", "case", "typedef", "message", "protocol", "ID")

  implicit def regex(rgx: Regex) = new Parser[String] {
    def apply(in: Input) = {
      val text = in.first.chars
      if (rgx.pattern.matcher(text).matches) Success(text, in.rest)
      else Error(text + " didn't match ´" + rgx + "´", in)
    }
  }

  def identifier = ident
  def number = """\d+""".r

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


  def test[T](input: String, parser: Parser[T] = specification) = parser(new lexical.Scanner(input)) match {
    case Success(res, _) => println("Success: " + res)
    case Failure(msg, _) => println("Failure: " + msg)
    case Error(msg, _) => println("Error: " + msg)
  }
  def testFile[T](file: String, parser: Parser[T] = specification) = test(io.Source.fromFile(file).mkString, parser)
  // def test(input: String) = parseAll(specification, input)
}