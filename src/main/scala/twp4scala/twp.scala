package twp4scala

import scala.Either
import java.lang.IllegalStateException
import java.io.{PushbackInputStream, IOException}
import tools.{DebugInput, Debugger}

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

/**
 * Can be used to perform Twp IO while automatically
 * reconnecting in case the connection is broken.
 */
abstract class PersistentConnection[T <: AbstractProtocol] {
  private var connection: Option[T] = None
  private var retryCount: Int = 0

  def open: T
  def close = {
    connection.foreach(_.shutdown)
    connection = None
  }

  def reconnect() {
    connection = Some(open)
    connection.foreach(_.initiate)
    Twp.log("Reconnecting " + this)
  }

  /**
   * Runs the given block using this connection.
   * Returns either an Exception, if one was thrown, or the actual result.
   */
  def apply[S](block: T => S): Either[Exception, S] = {
    if (!connection.isDefined) {
      reconnect()
    }
    try {
      Right(block(connection.get))
    } catch {
      case e: IOException if retryCount == 0 => { // prolly connection broken, try once again
        connection = None
        retryCount += 1
        apply(block)
      }
      case e: Exception => {
        retryCount = 0
        e.printStackTrace()
        Left(e)
      }
    }
  }

  def get[S](twpResult: Either[Exception, S]): S = {
    val error = twpResult.left.toOption
    error map { err =>
      if (Twp.debug) err.printStackTrace();
      else println(err.getMessage);
      null.asInstanceOf[S]} getOrElse twpResult.right.get
  }

  /**
   * While #apply will return either an Exception or the actual result,
   * get will return the result or null if an Exception was thrown.
   *
   * The Exception's error message will be printed to stdout.
   */
  def get[S](block: T => S): S = get(apply(block))
}

import Twp.{logr, logw, log}
import tools.GayString._

trait AbstractProtocol extends Connection {
  def initiate
  def shutdown
}

trait Protocol extends AbstractProtocol with TwpReader with TwpWriter {

  def protocolId: Int

  /**
   * Can push back exactly one byte for checking message tags.
   */
  abstract override lazy val in: Input =
    if (!Twp.debug) new Input(super.in, 16)
    else new DebugInput(super.in, 16)

  def shutdown = close

  def ! (msg: Message) {
    if (Twp.debug) {
      Twp.debug = false
      val data = msg.write.map(_.mkString(" ")).mkString(" | ")
      println("[DEBUG] Sending message: " + msg)
      println("[DEBUG]                  " + "^(\\d+) \\|".r.replaceAllIn(data,
        m => paint(m.group(1), Magenta) + " |") + " | " + paint("0", Magenta))
      Twp.debug = true
      println("[debug] Sending " + msg.write.flatten.mkString(" "))
    }
    msg.write.foreach(out.write)
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
  def checkDefined(implicit in: Input): Unit = ()
  def checkComplete(implicit in: Input): Unit = ()

  def read(implicit in: Input): T
  def isDefinedAt(implicit in: Input): Boolean
}

trait TwpWritable {
  def write: Stream[Array[Byte]]
}

trait Message extends TwpWriter with TwpWritable with TwpConversions {
  val End: Stream[Array[Byte]] = Stream(Array(0.toByte))
}

trait MessageCompanion[S <: Message, T] extends TwpReader
    with TwpWriter with TwpReadable[T] with TwpConversions { self =>

  def unapply(in: Input): Option[T] = {
    if (isDefinedAt(in)) {
      checkDefined(in)
      val result = Some(read(in))
      checkComplete(in)
      result
    } else None
  }

  def apply(values: T): S

  def isDefinedAt(implicit in: Input): Boolean = Preview.check(tag + 4 ==)

  override def checkDefined(implicit in: Input): Unit = expect(message(tag), Some("message " + tag + " expected"))
  /**
   * Checks whether or not the Message has been completely read. Expected to throw an Exception if not.
   */
  override def checkComplete(implicit in: Input): Unit = expect(endOfContent, None)(in)

  def tag: Int
  def read(implicit in: Input): T

  implicit def save(msg: S): Array[Byte] = msg.write.reduceLeft(_ ++ _)
  implicit def in(implicit input: Input): S = apply(read(input))

  implicit val toReader: TwpReadable[S] = new TwpReadable[S] {
    def read(implicit input: Input) = in
    def isDefinedAt(implicit input: Input) = self.isDefinedAt
    override def checkDefined(implicit in: Input) = self.checkDefined
    override def checkComplete(implicit in: Input) = self.checkComplete
  }
}

trait EmptyMessageCompanion[S <: Message] extends TwpReader
    with TwpReadable[S] with TwpWriter with TwpConversions { self: S =>

  def unapply(in: Input): Boolean =
    if (isDefinedAt(in)) {
      checkDefined(in)
      checkComplete(in)
      true
    } else false

  def apply() = this

  def tag: Int

  def isDefinedAt(implicit in: Input): Boolean = Preview.check(tag + 4 ==)

  override def checkDefined(implicit in: Input): Unit = expect(message(self.tag),
    Some("empty message " + self.tag + " expected"))
  /**
   * Checks whether or not the Message has been completely read. Expected to throw an Exception if not.
   */
  override def checkComplete(implicit in: Input): Unit = expect(endOfContent, None)(in)

  implicit def save(msg: S): Array[Byte] = msg.write.reduceLeft(_ ++ _)
  implicit def in(implicit input: Input): S = apply()

  def read(implicit in: Input): S = apply()
}

trait AppType[T] extends Message {

  val value: T

  def write(appType: AppTypeCompanion[_, T], data: Array[Byte]) =
    Stream(appType.tag.getBytes(1) ++ data.size.getBytes() ++ data)

  def get: T = value

  def canEqual(obj: Any): Boolean = obj.isInstanceOf[AppType[_]]

  override def equals(obj: Any): Boolean = obj match {
    case other: AppType[_] if other.canEqual(this) => value == other.value
    case _ => false
  }

  override def hashCode = 41 * value.hashCode
}

abstract class AppTypeCompanion[S <: AppType[T], T] extends MessageCompanion[S, T] {

  override def isDefinedAt(implicit input: Input) = Preview.check(tag ==)
  override def checkDefined(implicit input: Input) = expect(myTag, Some("application type " + tag))

  override def checkComplete(implicit in: Input): Unit = ()

  def read(reader: ((Input, Int) => T))(implicit in: Input): T = {
    val size = in.take(4).toInt
    reader(in, size)
  }

  private lazy val myTag = this.tag
}

class ErrorMessage(val failedMsgType: Int, val error: String) extends Message {
  def write = message(ErrorMessage.tag) #:: convert(failedMsgType).out #:: error.out #:: End
}

object ErrorMessage extends MessageCompanion[ErrorMessage, (Int, String)] {
  def tag = 8
  def apply(failedMsgType: Int, error: String) = new ErrorMessage(failedMsgType, error)
  def apply(in: Input, error: String) = new ErrorMessage(in.read, error)
  def apply(values: (Int,  String)) = new ErrorMessage(values._1, values._2)
  def read(implicit in: Input) = (someInt, string)
}

class Tag(val value: Int)

object Tag extends TwpReader {
  def apply(msg: Int) = new Tag(msg)
  def unapply(in: Input): Option[Int] = {
    val msg = in.read
    if (msg >= 0) Some(msg)
    else None
  }
  def isDefinedAt(implicit in: Input): Boolean = true
}

trait TwpConversions extends TwpWriter {

  implicit def convert[T](value: T)(implicit write: (T) => Raw) = TwpConverter(value)

  implicit def writeShortInt(i: Int) = Raw(someInt(i))
  implicit def writeLongInt(l: Long) = Raw(longInt(l.asInstanceOf[Int]))

  implicit def writeString(str: String) = Raw(string(str))

  implicit def writeSequence[T](seq: Seq[T])(implicit write: (T) => Raw): Raw =
    Raw(sequence ++ seq.map(write(_).data).flatten ++ endOfContent)

  implicit def writeAny(any: Any): Raw = Raw(any match {
    case Raw(data) => data
    case b: Array[Byte] => binary(b)
    case a: TwpWritable => a.write.reduceLeft(_ ++ _)
    case i: Int => someInt(i)
    case l: Long => longInt(l.asInstanceOf[Int])
    case s: String => string(s)
    case s: Seq[_] => {
      if (!s.isEmpty) {
        s.head match {
          case i: Int => writeSequence(s.asInstanceOf[Seq[Int]]).data
          case l: Long => writeSequence(s.asInstanceOf[Seq[Long]]).data
          case e: String => writeSequence(s.asInstanceOf[Seq[String]]).data
          case e: TwpWritable => sequence(s.asInstanceOf[Seq[TwpWritable]])
          case e => throw new IllegalStateException("Cannot write " + e + " of Seq " + s)
        }
      } else sequence(Nil)
    }
    case u: Unit => noValue
    case Unit => noValue
    case p: Product => Any(p.productIterator.toSeq: _*).write.flatten.toArray
    case _ => throw new IllegalStateException("Cannot write " + any + " (" + any.getClass + ")")
  })

  implicit def writeMessage(tag: Int) = new {
    def msg = message(tag)
    def raw = tag.getBytes(1)
  }

  implicit def writeExplicitInt(i: Int) = new {
    def short = shortInt(i)
    def long = longInt(i)
  }
}
object TwpConversions extends TwpConversions

case class TwpConverter[T](value: T)(implicit write: (T) => Raw) {
  def out: Array[Byte] = write(value).data
}

trait Preview {
  def check(p: Int => Boolean)(implicit in: Input) = {
    val tag = TwpReader.tag
    try { p(tag) } finally { in.unread(tag) }
  }
}
object Preview extends Preview

object String {
  def unapply(in: Input): Option[String] = {
    val str = TwpReader.tryString(in)
    str.left.foreach(in.unread)
    str.right.toOption
  }
}

object Int {
  def unapply(in: Input): Option[Int] = {
    val str = TwpReader.trySomeInt(in)
    str.left.foreach(in.unread)
    str.right.toOption
  }
}

object Long {
  def unapply(in: Input): Option[Long] = {
    val str = TwpReader.tryLongInt(in)
    str.left.foreach(in.unread)
    str.right.toOption.map(_.asInstanceOf[Long])
  }
}

object Binary {
  def unapply(in: Input): Option[Array[Byte]] = {
    val str = TwpReader.tryBinary(in)
    str.left.foreach(in.unread)
    str.right.toOption
  }
}

case class Raw(val data: Array[Byte])

trait SequenceReader[S <: Seq[T], T] extends TwpReadable[S] with TwpReader {
  def map(in: Input): T
  def canMap(in: Input): Boolean

  def read(implicit in: Input): S = readSequence(map).asInstanceOf[S]

  def isDefinedAt(implicit in: Input): Boolean = {
    val tag = in.read
    try {
      tag == 3 && (Preview.check(0 ==) || canMap(in))
    } finally {
      in.unread(tag)
    }
  }

  def readSequence[T](reader: (Input) => T)(implicit in: Input): Seq[T] = {
    expect(3, Some("Sequence"))
    Iterator.continually(in.read).takeWhile(0 !=).map(in.unread).map(_ => reader(in)).toList
  }
}

abstract class SeqCompanion[S <: Seq[T], T](implicit elementReader: TwpReadable[T]) extends TwpReader with TwpConversions {

  lazy val toReader: TwpReadable[S] = new TwpReadable[S] {
    val reader: TwpReadable[Seq[T]] = seqReader(elementReader)
    def isDefinedAt(implicit in: twp4scala.Input): Boolean = reader.isDefinedAt
    def read(implicit in: twp4scala.Input): S = reader.read.asInstanceOf[S]
  }

  implicit def makeReader[S <: Seq[T], T](seqCompanion: SeqCompanion[S, T]) = seqCompanion.toReader

  def in(implicit in: twp4scala.Input): S = toReader.read(in)

  def unapplySeq(in: Input): Option[List[T]] =
    if (toReader.isDefinedAt(in)) Some(toReader.read(in).toList)
    else None
}
