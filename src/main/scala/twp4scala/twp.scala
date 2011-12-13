package twp4scala

import scala.Either
import java.lang.IllegalStateException

object Twp {
  def apply[T <: AbstractProtocol, S](proto: T)(block: T => S): Either[Exception, S] = {
    try {
      proto.initiate
      Right(block(proto))
    } catch {
      case e: Exception => Left(e)
    } finally {
      proto.shutdown
    }
  }
}

trait AbstractProtocol extends Connection {
  def initiate
  def shutdown
}

trait Protocol extends AbstractProtocol with TwpReader with TwpWriter {

  def protocolId: Int

  /**
   * Can push back exactly one byte for checking message tags.
   */
  abstract override lazy val in = new Input(super.in, 1)

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

trait TwpReadable[T] {
  def read(implicit in: Input): T
}

trait TwpWritable {
  def write: Stream[Array[Byte]]
}

trait Message extends TwpWriter with TwpWritable with TwpConversions

trait MessageCompanion[S <: Message, T] extends TwpReader with TwpWriter with TwpReadable[T] with TwpConversions {
  def unapply(in: Input): Option[T] = {
    if (isDefinedAt(in)) {
      val result = Some(read(in))
      checkComplete(in)
      result
    } else None
  }

  def apply(values: T): S

  /**
   * Checks whether this kind of message can be read from the given PushbackInputStream.
   * Any given implementation must not consume any data from the stream if the result is negative.
   * That is all read bytes must be pushed back in that case.
   */
  def isDefinedAt(implicit in: Input): Boolean =
    Some(message).filter(tag !=).map(in.unread).map(_ => false) getOrElse true

  /**
   * Checks whether or not the Message has been completely read. Expected to throw an Exception if not.
   */
  def checkComplete(in: Input): Unit = expect(endOfContent, None)(in)

  def tag: Int
  def read(implicit in: Input): T

  implicit def save(msg: S): Array[Byte] = msg.write.reduceLeft(_ ++ _)
  implicit def in(implicit input: Input): S = apply(read(input))
}

trait EmptyMessageCompanion[S <: Message] extends TwpReader with TwpWriter with TwpConversions { this: S =>
  def unapply(in: Input): Boolean = isDefinedAt(in)

  def apply() = this

  def tag: Int

  /**
   * Checks whether this kind of message can be read from the given PushbackInputStream.
   * Any given implementation must not consume any data from the stream if the result is negative.
   * That is all read bytes must be pushed back in that case.
   */
  def isDefinedAt(implicit in: Input): Boolean =
    Some(message).filter(tag !=).map(in.unread).map(_ => false) getOrElse true

  /**
   * Checks whether or not the Message has been completely read. Expected to throw an Exception if not.
   */
  def checkComplete(in: Input): Unit = expect(endOfContent, None)(in)

  implicit def save(msg: S): Array[Byte] = msg.write.reduceLeft(_ ++ _)
  implicit def in(implicit input: Input): S = apply()
}

trait Struct extends Message

trait StructCompanion[S <: Struct, T] extends MessageCompanion[S, T] {
  def tag = -1 // not used
  override def isDefinedAt(implicit in: Input) = true
}

class ErrorMessage(val failedMsgType: Int, val error: String) extends Message {
  def write = message(ErrorMessage.tag) #:: failedMsgType #:: error #:: Output
}

object ErrorMessage extends MessageCompanion[ErrorMessage, (Int, String)] {
  def tag = 8
  def apply(failedMsgType: Int, error: String) = new ErrorMessage(failedMsgType, error)
  def apply(in: Input, error: String) = new ErrorMessage(in.read, error)
  def apply(values: (Int,  String)) = new ErrorMessage(values._1, values._2)
  def read(implicit in: Input) = (someInt, string)
}

class Tag(msg: Int)

object Tag extends TwpReader {
  def apply(msg: Int) = new Tag(msg)
  def unapply(in: Input): Option[Int] = {
    val msg = message(in)
    if (msg >= 0) Some(msg)
    else None
  }
}

trait TwpConversions extends TwpWriter {
  implicit def writeStringSequence(seq: Seq[String]): Array[Byte] = seq.flatMap(string).toArray
  implicit def writeString(str: String): Array[Byte] = string(str)
  
  implicit object stringReader extends SequenceReader[String, Seq[String]] {
    def map(in: Input) = string(in)
  }
  
  implicit def writeMessage(tag: Int) = new {
    def msg = message(tag)
  }
  
  implicit def writeInt(i: Int) = someInt(i)
  implicit def writeExplicitInt(i: Int) = new {
    def short = shortInt(i)
    def long = longInt(i)
  }

  implicit def writeAny(any: Any): Array[Byte] = any match {
    case a: TwpWritable => a.write.reduceLeft(_ ++ _)
    case i: Int => someInt(i)
    case s: String => string(s)
    case s: Seq[_] => {
      if (!s.isEmpty) {
        val head = s.head
        head match {
          case e: String => writeStringSequence(s.asInstanceOf[Seq[String]])
          case e: TwpWritable => sequence(s.asInstanceOf[Seq[TwpWritable]])
          case e => throw new IllegalStateException("Cannot write " + e + " of Seq " + s)
        }
      }
    }
    case _ => throw new IllegalStateException("Cannot write " + any)
  }
}

trait SequenceReader[T, S >: Seq[T]] extends TwpReadable[S] with TwpReader {
  def map(in: Input): T
  
  def read(implicit in: Input): S = readSequence(map)

  def readSequence[T](reader: (Input) => T)(implicit in: Input): Seq[T] =
    Iterator.continually(in.read).takeWhile(0 !=).map(in.unread).map(_ => reader(in)).toSeq
}

object TwpConversions extends TwpConversions

trait TwpReader extends ByteOperations {

  def tag(implicit in: Input) = in.read

  def expect(expected: Int, msg: Option[String])(implicit in: Input): Int = {
    val actual = in.read
    val info = if (msg != null) "(" + msg + ")" else ""
    if (expected == actual) actual
    else throw new RuntimeException("Expected " + expected + " " + info + ", got: " + actual)
  }
  def expect(expected: Array[Byte], msg: Option[String])(implicit in: Input): Int =
    expect(expected.head.toInt, msg)(in)

  def message(implicit in: Input) = {
    in.read - 4
  }

  def string(implicit in: Input) = in.read match {
    case short if short >= 17 && short <= 126 => {
      new String(in.take(short - 17), "UTF-8")
    }
    case long if long == 127 => {
      val size = in.take(4).toInt
      new String(in.take(size), "UTF-8")
    }
    case tag => throw new RuntimeException("Expected string, got: " + tag)
  }

  def shortInt(implicit in: Input) = in.read match {
    case 13 => in.read
    case tag => throw new RuntimeException("Expected short int, got: " + tag)
  }

  def longInt(implicit in: Input) = in.read match {
    case 14 => in.take(4).toInt
    case tag => throw new RuntimeException("Expected long int, got: " + tag)
  }

  def someInt(implicit in: Input) = in.read match {
    case 13 => in.read
    case 14 => in.take(4).toInt
    case tag => throw new RuntimeException("Expected int, got: " + tag)
  }

  def sequence[S](implicit in: Input, reader: TwpReadable[S]): S = reader.read(in)

  def peek(in: Input): Int = {
    val byte = in.read
    in unread byte
    byte
  }

  def any(implicit in: Input): Input = in

  /**
   * Checks whether the input starts with the given tag.
   * If so, the tag is consumed and the method returns true.
   * If not, the tag is unread and the method returns false.
   */
  def startsWith(tag: Int, in: Input): Boolean =
    Some(message(in)).filter(tag !=).map(in.unread).map(_ => false) getOrElse true
}

trait TwpWriter extends ByteOperations {

  def tag(tagType: Int) = Array(tagType.toByte)

  def endOfContent = Array(0.toByte)
  def noValue = Array(1.toByte)
  
  def sequence = Array(3.toByte)
  
  def sequence[T <: TwpWritable](seq: Seq[T]): Array[Byte] =
    seq.flatMap(_.write).flatten.toArray ++ endOfContent

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