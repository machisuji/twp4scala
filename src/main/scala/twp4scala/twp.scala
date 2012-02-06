package twp4scala

import scala.Either
import java.lang.IllegalStateException
import tools.MemoryProtocol

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

trait TwpReadable[T] {
  def checkDefined(implicit in: Input): Unit = ()
  def checkComplete(implicit in: Input): Option[LooseStruct] = None

  def read(implicit in: Input): T
  def isDefinedAt(implicit in: Input): Boolean
}

trait TwpWritable {
  def write: Stream[Array[Byte]]
}

trait CanBeExtension extends TwpWritable {
  def isExtensionLike: Boolean
}

trait Message extends TwpWriter with CanBeExtension with TwpConversions {
  val End: Stream[Array[Byte]] = Stream(Array(0.toByte))

  def isExtensionLike = write.headOption.exists(_.headOption.exists(12 ==))
}

trait MessageCompanion[S <: Message, T] extends TwpReader with TwpWriter with TwpReadable[T] { self =>

  def isExtension = tag > 7

  def unapply(in: Input): Option[T] = {
    if (isDefinedAt(in)) {
      checkDefined(in)
      val result = Some(read(in))
      in.lastExtension = checkComplete(in)
      result
    } else None
  }

  def apply(values: T): S

  def isDefinedAt(implicit in: Input): Boolean = Preview.check(byte =>
    if (isExtension) byte == 12 && Preview.check(4, num => num.toSeq == tag.getBytes().toSeq) else tag + 4 == byte)

  override def checkDefined(implicit in: Input): Unit = expect(message(tag), Some("message " + tag + " expected"))
  /**
   * Checks whether or not the Message has been completely read. Expected to throw an Exception if not.
   */
  override def checkComplete(implicit in: Input): Option[LooseStruct] = {
    if (Preview.check(0 !=) && LooseStruct.isDefinedAt) {
      try {
        Some(LooseStruct.read)
      } finally {
        if (Preview.check(0 ==)) {
          expect(0, None)
        }
      }
    } else {
      expect(endOfContent, None)(in)
      None
    }
  }

  def tag: Int
  def read(implicit in: Input): T

  /**
   * Tries to build a Message from the given Struct.
   * Works only, if the Struct itself represents an extension message, meaning that it has to
   * have an extension ID.
   */
  def from(struct: LooseStruct): Option[S] = unapply(MemoryProtocol(struct.write.flatten.toArray).in).map(apply)

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
  override def checkComplete(implicit in: Input): Option[LooseStruct] = {
    expect(endOfContent, None)(in)
    None
  }

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

  override def checkComplete(implicit in: Input): Option[LooseStruct] = None

  def read(reader: ((Input, Int) => T))(implicit in: Input): T = {
    val size = in.take(4).toInt
    reader(in, size)
  }

  private lazy val myTag = this.tag
}

class ErrorMessage(val failedMsgType: Int, val error: String) extends Message {
  def write = message(ErrorMessage.tag) #:: someInt(failedMsgType) #:: string(error) #:: End
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

  implicit def writeTwpWritable(writable: TwpWritable) = Raw(writable.write.flatten.toArray)

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
    val tag = in.read
    try { p(tag) } finally {
      if (tag != -1) in.unread(tag)
    }
  }
  def check(size: Int, p: Array[Byte] => Boolean)(implicit in: Input) = {
    val bytes = new Array[Byte](size)
    val read = in.read(bytes)
    try { p(bytes) } finally { in.unread(bytes.take(read)) }
  }
}
object Preview extends Preview

object string {
  def unapply(in: Input): Option[String] = {
    val str = TwpReader.tryString(in)
    str.left.foreach(in.unread)
    str.right.toOption
  }
}

object int {
  def unapply(in: Input): Option[Int] = {
    val str = TwpReader.trySomeInt(in)
    str.left.foreach(in.unread)
    str.right.toOption
  }
}

object long {
  def unapply(in: Input): Option[Long] = {
    val str = TwpReader.tryLongInt(in)
    str.left.foreach(in.unread)
    str.right.toOption.map(_.asInstanceOf[Long])
  }
}

object binary {
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
