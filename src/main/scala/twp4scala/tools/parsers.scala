package twp4scala.tools

import scala.util.parsing.combinator.syntactical._
import scala.util.matching.Regex

trait RegexParsers { this: TokenParsers =>
  
  class FullMatch(val regex: Regex, errorMsg: Option[String]) extends Parser[String] {
    def apply(in: Input) = {
      val text = in.first.chars
      if (regex.pattern.matcher(text).matches) Success(text, in.rest)
      else errorMsg.orElse(Some("\":text\" didn't match /:regex/")).map(_
        .replace(":token", text)
        .replace(":regex", regex.toString)
      ).map(Error(_, in)).get
    }
  }

  
  implicit def regex(rgx: Regex) = new FullMatch(rgx, None)

  def regex(rgx: String, error: String = null) = {
    val errorMsg = if (error != null) Some(error) else None
    new FullMatch(rgx.r, errorMsg)
  }
}