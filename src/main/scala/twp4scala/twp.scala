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

  var debug = false

  private[twp4scala] def logw(msg: String) = {
    if (Twp.debug) println("[DEBUG] write " + msg)
  }
  private[twp4scala] def logr(msg: String) = {
    if (Twp.debug) println("[DEBUG] read " + msg)
  }
  private[twp4scala] def log(msg: String) = {
    if (Twp.debug) println("[DEBUG] " + msg)
  }
}

import Twp.{logr, logw, log}

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

trait Message extends TwpWriter with TwpWritable with TwpConversions {
  val End: Stream[Array[Byte]] = Stream.empty
}

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
  implicit def writeStringSequence(seq: Seq[String]): Array[Byte] = {
    logw("Seq[String]: " + seq.toString)
    sequence ++ seq.flatMap(string).toArray[Byte] ++ endOfContent
  }
  implicit def writeString(str: String): Array[Byte] = string(str)
  
  implicit object stringReader extends SequenceReader[String, Seq[String]] {
    def map(in: Input) = string(in)
  }
  
  implicit def writeMessage(tag: Int) = new {
    def msg = message(tag)
    def raw = tag.getBytes(1)
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
    case u: Unit => noValue
    case _ => throw new IllegalStateException("Cannot write " + any)
  }
}

trait SequenceReader[T, S >: Seq[T]] extends TwpReadable[S] with TwpReader {
  def map(in: Input): T
  
  def read(implicit in: Input): S = readSequence(map)

  def readSequence[T](reader: (Input) => T)(implicit in: Input): Seq[T] = {
    expect(3, Some("Sequence"))
    Iterator.continually(in.read).takeWhile(0 !=).map(in.unread).map(_ => reader(in)).toSeq
  }
}

object TwpConversions extends TwpConversions

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

  def message(implicit in: Input) = {
    val res = in.read
    if (res >= 0 && res <= 11) {
      val ret = res - 4
      logr("message " + ret); ret
    }
    else if (res == 12) {
      val ret = in.take(4).toInt
      logr("extension " + ret); ret
    } 
    else throw new RuntimeException("Expected message/extension, got: " + res)
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

trait TwpWriter extends ByteOperations {

  def tag(tagType: Int) = {
    log("tag(%d)" format tagType)
    Array(tagType.toByte)
  }

  def endOfContent = {
    log("endOfContent")
    Array(0.toByte)
  }
  def noValue = {
    log("noValue")
    Array(1.toByte)
  }

  def sequence: Array[Byte] = {
    log("sequence")
    Array(3.toByte)
  }

  def sequence[T <: TwpWritable](seq: Seq[T]): Array[Byte] = {
    logw(seq.toString)
    this.sequence ++ (seq.flatMap(_.write).flatten.toArray ++ endOfContent)
  }

  def message(id: Int): Array[Byte] = {
    if (id >= 0 && id <= 7) {
      logw("message(%d) (%d)" format (id,  id + 4))
      (4 + id).getBytes(1)
    } else { // send extension
      logw("extension(%d)" format id)
      12.getBytes(1) ++ id.getBytes()
    }
  }

  def shortInt(i: Int) = {
    logw("shortInt(%d)" format i)
    tag(13) ++ i.getBytes(1)
  }
  def longInt(i: Int) = {
    logw("longInt(%d)" format i)
    tag(14) ++ i.getBytes(4)
  }

  def someInt(i: Int) = if (i >= 256) longInt(i) else shortInt(i)

  def string(str: String): Array[Byte] = {
    logw("String(%s)" format str)
    val data = str.getBytes("UTF-8")
    val (msgTag, prefix) =
      if (data.size <= 109) tag(17 + data.size) -> Array[Byte]()
      else tag(127) -> data.size.getBytes()
    msgTag ++ prefix ++ data
  }

  def binary(data: Array[Byte]): Array[Byte] = {
    logw("binary(%s)" format data.toString)
    val (msgTag, prefix) =
      if (data.size <= 0xFF) tag(15) -> data.size.getBytes(1)
      else tag(16) -> data.size.getBytes(4)
    msgTag ++ prefix ++ data
  }

  def nop = {
    log("nop")
    new Array[Byte](0)
  }
}