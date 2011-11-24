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

  /**
   * Can push back exactly one byte for checking message tags.
   */
  abstract override lazy val in = new PushbackInputStream(super.in, 1)

  def shutdown = close

  def ! (msg: Message[P]) = {
    msg write out
    out write endOfContent
    out.flush
  }

  def receive[R](reader: PartialFunction[PushbackInputStream, R]) = {
    val data = in.read; in unread data
    val forDefinedAt = new ByteArrayInputStream(Array(data.toByte))
    val input = new PushbackInputStream(new SequenceInputStream(forDefinedAt, in), 1)

    reader orElse new PartialFunction[PushbackInputStream, R] {
      def isDefinedAt(in: PushbackInputStream) = true
      def apply(in: PushbackInputStream): R = throw new RuntimeException("Unexpected response starting with " + in.read)
    } apply input
  }
}

trait Message[P <: Protocol[P]] extends TwpWriter {
  def write(out: OutputStream): Unit
}

trait MessageCompanion[P <: Protocol[P], M <: Message[P], T] extends TwpReader with TwpWriter {
  def unapply(in: PushbackInputStream): Option[T] = {
    if (isDefinedAt(in)) {
      if (in.available() == 0) Some(null.asInstanceOf[T]) // hack to make partial matches work (receive)
      else {
        val result = Some(read(in))
        expect(endOfContent, None)(in); result
      }
    } else None
  }
  def isDefinedAt(implicit in: InputStream): Boolean
  def read(implicit in: InputStream): T
}

trait TwpReader extends ByteOperations {

  def tag(implicit in: InputStream) = in.read

  def expect(expected: Int, msg: Option[String])(implicit in: InputStream): Int = {
    val actual = in.read
    val info = if (msg != null) "(" + msg + ")" else ""
    if (expected == actual) actual
    else throw new RuntimeException("Expected " + expected + " " + info + ", got: " + actual)
  }
  def expect(expected: Array[Byte], msg: Option[String])(implicit in: InputStream): Int = expect(expected.head.toInt, msg)(in)

  def message(implicit in: InputStream) = {
    val msg = in.read - 4
    if (0 <= msg && msg <= 7) msg
    else throw new RuntimeException("Expected message, got: " + msg)
  }

  def string(implicit in: InputStream) = in.read match {
    case short if short >= 17 && short <= 126 => {
      new String(in.take(short - 17), "UTF-8")
    }
    case long if long == 127 => {
      val size = in.take(4).toInt
      new String(in.take(size), "UTF-8")
    }
    case tag => throw new RuntimeException("Expected string, got: " + tag)
  }

  def shortInt(implicit in: InputStream) = in.read match {
    case 13 => in.read
    case tag => throw new RuntimeException("Expected short int, got: " + tag)
  }

  def longInt(implicit in: InputStream) = in.read match {
    case 14 => in.take(4).toInt
    case tag => throw new RuntimeException("Expected long int, got: " + tag)
  }

  def int(implicit in: InputStream) = in.read match {
    case 13 => in.read
    case 14 => in.take(4).toInt
    case tag => throw new RuntimeException("Expected int, got: " + tag)
  }

  def test(implicit in: InputStream): Array[Byte] = Array()
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