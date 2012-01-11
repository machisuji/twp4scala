package twp4scala

import scala.Either
import java.lang.IllegalStateException
import java.io.IOException

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
  abstract override lazy val in = new Input(super.in, 1)

  def shutdown = close

  def ! (msg: Message) {
    if (Twp.debug) {
      Twp.debug = false
      val data = msg.write.map(_.mkString(" ")).mkString(" | ")
      println("[DEBUG] Sending message: " + msg)
      println("[DEBUG]                  " + "^(\\d+) \\|".r.replaceAllIn(data,
        m => paint(m.group(1), Magenta) + " |") + " | " + paint("0", Magenta))
      Twp.debug = true
    }
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
  def isDefinedAt(implicit in: Input): Boolean
}

trait TwpWritable {
  def write: Stream[Array[Byte]]
}

trait Message extends TwpWriter with TwpWritable with TwpConversions {
  val End: Stream[Array[Byte]] = Stream.empty
}

trait MessageCompanion[S <: Message, T] extends TwpReader
    with TwpWriter with TwpReadable[T] with TwpConversions { self =>

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
    Some(message(in, failSilently = true)).filter(tag !=).map(in.unread).map(_ => false) getOrElse true

  /**
   * Checks whether or not the Message has been completely read. Expected to throw an Exception if not.
   */
  def checkComplete(in: Input): Unit = expect(endOfContent, None)(in)

  def tag: Int
  def read(implicit in: Input): T

  implicit def save(msg: S): Array[Byte] = msg.write.reduceLeft(_ ++ _)
  implicit def in(implicit input: Input): S = apply(read(input))

  implicit val toReader: TwpReadable[S] = new TwpReadable[S] {
    def read(implicit input: Input) = in
    def isDefinedAt(implicit input: Input) = self.isDefinedAt
  }
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
    Some(message(in, failSilently = true)).filter(tag !=).map(in.unread).map(_ => false) getOrElse true

  /**
   * Checks whether or not the Message has been completely read. Expected to throw an Exception if not.
   */
  def checkComplete(in: Input): Unit = expect(endOfContent, None)(in)

  implicit def save(msg: S): Array[Byte] = msg.write.reduceLeft(_ ++ _)
  implicit def in(implicit input: Input): S = apply()
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

  override def checkComplete(in: Input): Unit = ()

  def read(reader: ((Input, Int) => T))(implicit in: Input): T = {
    require(tag(in) == myTag, "Application Type %d expected".format(myTag))
    val size = in.take(4).toInt
    reader(in, size)
  }

  private lazy val myTag = this.tag
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
  implicit def writeA[T](value: T)(implicit writer: (T) => TwpWritable): Array[Byte] =
    writer(value).write.flatten.toArray[Byte]

  implicit def writeSequence[T](seq: Seq[T])(implicit writer: (T) => TwpWritable): Array[Byte] =
    sequence ++ seq.map(writer).flatMap(_.write).flatten.toArray[Byte] ++ endOfContent

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

trait Preview {
  def check(p: Int => Boolean)(implicit in: Input) =
    Some(TwpReader.tag).filterNot(p).map(in.unread).map(_ => false) getOrElse true
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
    val str = TwpReader.tryShortInt(in)
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

  def in(implicit in: twp4scala.Input): S = toReader.read(in)

  def unapplySeq(in: Input): Option[List[T]] =
    if (toReader.isDefinedAt(in)) Some(toReader.read(in).toList)
    else None
}
