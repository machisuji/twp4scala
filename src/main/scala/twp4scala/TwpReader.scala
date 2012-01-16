package twp4scala

import tools.Debugger
import Twp.{log, logr}

trait TwpReader extends ByteOperations {

  def in[T](implicit reader: TwpReadable[T], in: Input): T = {
    reader.checkDefined
    val ret = reader.read
    reader.checkComplete
    ret
  }
  def is[T](implicit reader: TwpReadable[T], in: Input): Boolean = reader.isDefinedAt

  def tag(implicit in: Input) = {
    val ret = in.read
    logr("tag " + ret); ret
  }

  def expect(expected: Int, msg: Option[String])(implicit in: Input): Int = {
    logr("expecting " + expected + " (" + Debugger.getLines(9).mkString(" <- ") + ")")
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

  def tryString(implicit in: Input) = in.read match {
    case short if short >= 17 && short <= 126 => {
      val ret = new String(in.take(short - 17), "UTF-8")
      logr("short string \"%s\"" format ret); Right(ret)
    }
    case long if long == 127 => {
      val size = in.take(4).toInt
      val ret = new String(in.take(size), "UTF-8")
      logr("long string \"%s\"" format ret); Right(ret)
    }
    case tag => Left(tag)
  }
  def string(implicit in: Input) = {
    val str = tryString
    str.left.foreach(tag => throw new RuntimeException("Expected string, got: " + tag))
    str.right.get
  }

  def tryShortInt(implicit in: Input) = in.read match {
    case 13 => {
      val ret = in.read
      logr("short int " + ret); Right(ret)
    }
    case tag => Left(tag)
  }
  def shortInt(implicit in: Input) = {
    val short = tryShortInt
    short.left.foreach(tag => throw new RuntimeException("Expected short int, got: " + tag))
    short.right.get
  }

  def tryLongInt(implicit in: Input) = in.read match {
    case 14 => {
      val ret = in.take(4).toInt
      logr("long int " + ret); Right(ret)
    }
    case tag => Left(tag)
  }
  def longInt(implicit in: Input) = {
    val long = tryLongInt
    long.left.foreach(tag => throw new RuntimeException("Expected long int, got: " + tag))
    long.right.get
  }

  def trySomeInt(implicit in: Input) = in.read match {
    case 13 => { in.unread(13); Right(shortInt) }
    case 14 => { in.unread(14); Right(longInt) }
    case tag => Left(tag)
  }
  def someInt(implicit in: Input) = {
    val int = trySomeInt
    int.left.foreach(tag => throw new RuntimeException("Expected int, got: " + tag))
    int.right.get
  }

  def tryBinary(implicit in: Input) = {
    val tg = tag
    val size =
      if (tg == 15) in.take(1).toInt
      else if (tg == 16) in.take(4).toInt
      else -1
    if (size != -1) {
      val ret = in.take(size)
      logr("binary " + ret); Right(ret)
    } else {
      Left(tg)
    }
  }
  def binary(implicit in: Input) = {
    val bin = tryBinary
    bin.left.foreach(tag => throw new RuntimeException("Expected binary, got: " + tag))
    bin.right.get
  }

  def peek(in: Input): Int = {
    val byte = in.read
    in unread byte
    byte
  }

  def any(implicit in: Input): Any = {
    val ret = Any.read
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
