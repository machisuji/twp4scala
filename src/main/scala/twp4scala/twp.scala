package twp4scala

import java.io._
import java.net._

trait AbstractProtocol extends Connection {
  def initiate
  def shutdown
}

trait Protocol[P <: Protocol[P]] extends AbstractProtocol with TwpReader with TwpWriter {

  def protocolId: Int

  def initiate {
    val init = "TWP3\n".getBytes("ISO-8859-1")

    out write init
    out write shortInt(protocolId)

    out.flush()
  }

  def peek(in: PushbackInputStream): Int = {
    val ret = in.read
    in.unread(1)
    ret
  }

  /**
   * Can push back exactly one byte for checking message tags.
   */
  abstract override lazy val in = new PushbackInputStream(super.in, 1)

  def shutdown = close

  def ! (msg: Message[P]) = {
    msg send this.asInstanceOf[P]
    out.flush
  }

  def receive[R](reader: PartialFunction[PushbackInputStream, R]) = {
    val data = in.read
    val forDefinedAt = new ByteArrayInputStream(Array(data.toByte))
    val input = new PushbackInputStream(new SequenceInputStream(forDefinedAt, in), 1)
    in unread data
    reader orElse new PartialFunction[PushbackInputStream, R] {
      def isDefinedAt(in: PushbackInputStream) = true
      def apply(in: PushbackInputStream): R = throw new RuntimeException("Unexpected response starting with " + in.read)
    } apply input
  }
}

trait Message[P <: Protocol[P]] {
  def send(p: P): Unit
}

trait MessageCompanion[P <: Protocol[P], M <: Message[P]] {
  def unapply(in: PushbackInputStream): Option[_]
}

trait TwpReader extends ByteOperations {

  def in: InputStream

  def tag = in.read

  def expect(expected: Int, msg: String = null): Int = {
    val actual = in.read
    val info = if (msg != null) "(" + msg + ")" else ""
    if (expected == actual) actual
    else throw new RuntimeException("Expected " + expected + " " + info + ", got: " + actual)
  }
  def expectBecause(expected: Int, because: String): Int = expect(expected, because)

  def expect(expected: Array[Byte]): Int = expect(expected.head.toInt)
  def expectBecause(expected: Array[Byte], because: String): Int = expect(expected.head.toInt, because)

  def message = {
    val msg = in.read - 4
    if (0 <= msg && msg <= 7) msg
    else throw new RuntimeException("Expected message, got: " + msg)
  }

  def string = in.read match {
    case short if short >= 17 && short <= 126 => {
      new String(in.take(short - 17), "UTF-8")
    }
    case long if long == 127 => {
      val size = in.take(4).toInt
      new String(in.take(size), "UTF-8")
    }
    case tag => throw new RuntimeException("Expected string, got: " + tag)
  }

  def shortInt = in.read match {
    case 13 => in.read
    case tag => throw new RuntimeException("Expected short int, got: " + tag)
  }

  def longInt = in.read match {
    case 14 => in.take(4).toInt
    case tag => throw new RuntimeException("Expected long int, got: " + tag)
  }
}

trait TwpWriter extends ByteOperations {

  def tag(tagType: Int) = Array(tagType.toByte)

  def endOfContent = Array(0.toByte)
  def noValue = Array(1.toByte)

  def message(id: Int): Array[Byte] =
    if (id >= 0 && id <= 7) (4 + id).getBytes(1)
    else throw new IllegalArgumentException("Message out of range (0 - 7): " + id)

  def shortInt(i: Int) = tag(13) ++ i.getBytes(1)
  def longInt(i: Int) = tag(14) ++ i.getBytes(4)

  def string(str: String): Array[Byte] = {
    val data = str.getBytes("UTF-8")
    val (msgTag, prefix) =
      if (data.size <= 109) tag(17 + data.size) -> Array[Byte]()
      else tag(127) -> data.size.getBytes()
    msgTag ++ prefix ++ data
  }

  def binary(data: Array[Byte]): Array[Byte] = {
    val (msgTag, prefix) =
      if (data.size <= 0xFF) tag(15) -> data.size.getBytes(1)
      else tag(16) -> data.size.getBytes(4)
    msgTag ++ prefix ++ data
  }
}