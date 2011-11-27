package twp4scala

import java.io.{Console => _, _}

trait AbstractProtocol extends Connection {
  def initiate
  def shutdown
}

trait Protocol extends AbstractProtocol with TwpReader with TwpWriter {

  def protocolId: Int

  /**
   * Can push back exactly one byte for checking message tags.
   */
  abstract override lazy val in = new PushbackInputStream(super.in, 1)

  def shutdown = close

  def ! (msg: Message) {
    msg.write.foreach(out write)
    out write endOfContent
    out.flush
  }
}

trait Client extends Protocol {
  def initiate {
    val init = "TWP3\n".getBytes("ISO-8859-1")

    out write init
    out write shortInt(protocolId)

    out.flush()
  }
}

trait Server extends Protocol {
  def initiate {
    val init = "TWP3\n".getBytes("ISO-8859-1").toSeq
    val intro = in.take(init.size).toSeq
    val pid = shortInt(in)

    if (init == intro) {
      if (pid != protocolId) {
        Console.err.println("Client talks unsupported protocol " + pid + ". Expected " + protocolId + ".")
        this ! ErrorMessage(-1, "Unsupported protocol (" + pid + ")")
      }
    } else {
      Console.err.println("Client doesn't speak TWP. Closing connection.")
      close
    }
  }
}

trait Message extends TwpWriter {
  def write: Stream[Array[Byte]]
}

trait MessageCompanion[T] extends TwpReader with TwpWriter {
  def unapply(in: PushbackInputStream): Option[T] = {
    if (isDefinedAt(in)) {
      val result = Some(read(in))
      expect(endOfContent, None)(in)
      result
    } else None
  }

  /**
   * Checks whether this kind of message can be read from the given PushbackInputStream.
   * Any given implementation must not consume any data from the stream if the result is negative.
   * That is all read bytes must be pushed back in that case.
   */
  def isDefinedAt(implicit in: PushbackInputStream): Boolean =
    Some(message).filter(tag !=).map(in.unread).map(_ => false) getOrElse true

  def tag: Int
  def read(implicit in: InputStream): T
}

class ErrorMessage(val failedMsgType: Int, val error: String) extends Message {
  def write: Stream[Array[Byte]] = {
    message(ErrorMessage.tag) #::
    someInt(failedMsgType) #::
    string(error) #:: Stream.empty
  }
}

object ErrorMessage extends MessageCompanion[(Int, String)] {
  def tag = 8
  def apply(failedMsgType: Int, error: String) = new ErrorMessage(failedMsgType, error)
  def apply(in: InputStream, error: String) = new ErrorMessage(in.read, error)
  def read(implicit in: InputStream) = (someInt, string)
}

class Tag(msg: Int)

object Tag extends TwpReader {
  def apply(msg: Int) = new Tag(msg)
  def unapply(in: PushbackInputStream): Option[Int] = {
    val msg = message(in)
    if (msg >= 0) Some(msg)
    else None
  }
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
    in.read - 4
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

  def someInt(implicit in: InputStream) = in.read match {
    case 13 => in.read
    case 14 => in.take(4).toInt
    case tag => throw new RuntimeException("Expected int, got: " + tag)
  }

  def peek(in: PushbackInputStream): Int = {
    val byte = in.read
    in unread byte
    byte
  }
}

trait TwpWriter extends ByteOperations {

  def tag(tagType: Int) = Array(tagType.toByte)

  def endOfContent = Array(0.toByte)
  def noValue = Array(1.toByte)

  def message(id: Int): Array[Byte] = (4 + id).getBytes(1)

  def shortInt(i: Int) = tag(13) ++ i.getBytes(1)
  def longInt(i: Int) = tag(14) ++ i.getBytes(4)

  def someInt(i: Int) = if (i >= 256) longInt(i) else shortInt(i)

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

  def nop = new Array[Byte](0)
}