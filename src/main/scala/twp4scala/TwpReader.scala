package twp4scala

import tools.ast.ApplicationType
import Twp.{log, logr}

trait TwpReader extends ByteOperations {

  def tag(implicit in: Input) = {
    val ret = in.read
    logr("tag " + ret); ret
  }

  def expect(expected: Int, msg: Option[String])(implicit in: Input): Int = {
    logr("expecting " + expected)
    val actual = /*if (expected == 0 && in.available == 0) 0 else*/ in.read
    val info = msg.map("(" + _ + ")").getOrElse("")
    logr("actually: " + actual)
    if (expected == actual) actual
    else throw new RuntimeException("Expected " + expected + " " + info + ", got: " + actual)
  }
  def expect(expected: Array[Byte], msg: Option[String])(implicit in: Input): Int =
    expect(expected.head.toInt, msg)(in)

  def message(implicit in: Input, failSilently: Boolean = false) = {
    val res = in.read
    if (res >= 4 && res <= 11) {
      val ret = res - 4
      logr("message " + ret); ret
    }
    else if (res == 12) {
      val ret = in.take(4).toInt
      logr("extension " + ret); ret
    } else if (failSilently) {
      log("Expected to read message/extension, got: " + res)
      res
    } else throw new IllegalStateException("Expected to read message/extension, got: " + res)
  }

  def string(implicit in: Input) = in.read match {
    case short if short >= 17 && short <= 126 => {
      val ret = new String(in.take(short - 17), "UTF-8")
      logr("short string \"%s\"" format ret); ret
    }
    case long if long == 127 => {
      val size = in.take(4).toInt
      val ret = new String(in.take(size), "UTF-8")
      logr("long string \"%s\"" format ret); ret
    }
    case tag => throw new RuntimeException("Expected string, got: " + tag)
  }

  def shortInt(implicit in: Input) = in.read match {
    case 13 => {
      val ret = in.read
      logr("short int " + ret); ret
    }
    case tag => throw new RuntimeException("Expected short int, got: " + tag)
  }

  def longInt(implicit in: Input) = in.read match {
    case 14 => {
      val ret = in.take(4).toInt
      logr("long int " + ret); ret
    }
    case tag => throw new RuntimeException("Expected long int, got: " + tag)
  }

  def someInt(implicit in: Input) = in.read match {
    case 13 => { in.unread(13); shortInt }
    case 14 => { in.unread(14); longInt }
    case tag => throw new RuntimeException("Expected int, got: " + tag)
  }

  def binary(implicit in: Input) = {
    val tg = tag
    val size =
      if (tg == 15) in.take(1).toInt
      else if (tg == 16) in.take(4).toInt
      else throw new RuntimeException("Expected binary, got: " + tag)

    val ret = in.take(size)
    logr("binary " + ret); ret
  }

  def sequence[S](implicit in: Input, reader: TwpReadable[S]): S = {
    val ret = reader.read(in)
    logr("sequence " + ret); ret
  }

  def peek(in: Input): Int = {
    val byte = in.read
    in unread byte
    byte
  }

  def any(implicit in: Input): Any = {
    val ret = TwpAny.read
    logr("any " + ret.toString); ret
  }

  /**
   * Checks whether the input starts with the given tag.
   * If so, the tag is consumed and the method returns true.
   * If not, the tag is unread and the method returns false.
   */
  def startsWith(tag: Int, in: Input): Boolean =
    Some(message(in)).filter(tag !=).map(in.unread).map(_ => false) getOrElse true
}

object TwpReader extends TwpReader
