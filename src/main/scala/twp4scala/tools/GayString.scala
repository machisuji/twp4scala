package twp4scala.tools

/**
 * Produces gay, that is colourful and/or otherwise fancy ANSI strings for Unix shells.
 */
object GayString {
  abstract class Type(val value: Int)
  abstract class Color(value: Int) extends Type(value)
  case object Black extends Color(30)
  case object Red extends Color(31)
  case object Green extends Color(32)
  case object Brown extends Color(33)
  case object Blue extends Color(34)
  case object Magenta extends Color(35)
  case object Cyan extends Color(36)
  case object White extends Color(37)

  def paint(msg: String, color: Color) =
    "\033[0;%dm%s\033[0m" format (color.value, msg)

  implicit def makeGayString(str: String) = new {
    def painted(color: Color) = paint(str, color)
  }
}