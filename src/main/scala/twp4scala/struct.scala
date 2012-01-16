package twp4scala

import java.lang.IllegalStateException


trait Struct extends Message

object Struct {
  val tag = 2
}

trait StructCompanion[S <: Struct, T] extends MessageCompanion[S, T] {
  def tag = Struct.tag
  override def checkDefined(implicit in: Input): Unit = expect(tag, Some("struct expected"))
  override def isDefinedAt(implicit in: Input) = Preview.check(tag ==)
}

class LooseStruct(val values: Seq[Any], extensionId: Option[Int] = None) extends TwpWriter with TwpConversions with TwpWritable {
  val End = Stream(Array(0.toByte))

  def map(names: Symbol*) = {
    val ns: Seq[(Symbol, Any)] = names.toSeq.zip(values)
    new DynamicStruct(Map[Symbol, Any](ns: _*))
  }

  protected def writeId: Array[Byte] = extensionId.map(id =>
    (12.toByte +: id.getBytes())).getOrElse(Array(2.toByte))

  def write: Stream[Array[Byte]] = {
    val data = values.map(TwpConversions.writeAny(_).data).flatten.toArray[Byte]
    writeId #:: data #:: End
  }

  override def toString = "LooseStruct(" + values.toString() + ")"

  def get[T] = values.head.asInstanceOf[T]
  def get[T1, T2]: (T1, T2) = (values(0).asInstanceOf[T1], values(1).asInstanceOf[T2])
  def get[T1, T2, T3]: (T1, T2, T3) =
    (values(0).asInstanceOf[T1], values(1).asInstanceOf[T2], values(2).asInstanceOf[T3])
  def get[T1, T2, T3, T4]: (T1, T2, T3, T4) =
    (values(0).asInstanceOf[T1], values(1).asInstanceOf[T2], values(2).asInstanceOf[T3], values(3).asInstanceOf[T4])
  def get[T1, T2, T3, T4, T5]: (T1, T2, T3, T4, T5) =
    (values(0).asInstanceOf[T1], values(1).asInstanceOf[T2], values(2).asInstanceOf[T3],
      values(3).asInstanceOf[T4], values(4).asInstanceOf[T5])

  def asExtension(id: Int) = new LooseStruct(values, Some(id))

  def isExtension = extensionId.isDefined
}

object LooseStruct extends TwpReader with TwpReadable[LooseStruct] with Preview {
  def read(implicit input: Input): LooseStruct = {
    val extId =
      if (check(Struct.tag ==)) {
        input.read
        None
      }
      else if (check(12 ==)) {
        input.read
        Some(input.take(4).toInt)
      }
      else throw new RuntimeException("Expected some sort of struct, got: " + input.read)
    val values = Iterator.continually(input.read).takeWhile(0 <).map(input.unread).map(_ => Any.read(input))
    new LooseStruct(values.toList, extId)
  }
  def isDefinedAt(implicit in: Input): Boolean = Preview.check(tag =>
    Struct.tag == tag || 12 == tag)
}

object Any extends TwpReader with TwpConversions with TwpReadable[Any] {

  def apply(values: Any*) = new LooseStruct(values.toSeq)

  def read(implicit input: Input): Any = {
    Some(input.read).map { tag =>
      if (tag == 1) None
      else {
        input.unread(tag)
        tag match {
          case 2 => LooseStruct.read
          case 12 => LooseStruct.read
          case 3 => in[Seq[Any]]
          case 13 => someInt
          case 14 => someInt
          case 15 => binary
          case 16 => binary
          case n if n >= 17 && n <= 127 => string
          case n => throw new IllegalStateException("Unexpected tag in struct: " + n)
        }
      }
    }.getOrElse(Unit)
  }

  def isDefinedAt(implicit in: Input): Boolean = Preview.check(tag => tag > 0 && tag < 128)

  implicit private val anySeqReader = new SeqReader

  class SeqReader extends SequenceReader[Seq[Any], Any] {
    def map(in: Input) = Any.read(in)
    def canMap(in: Input) = Any.isDefinedAt(in)
  }
}

case class DynamicStruct(val values: Map[Symbol, Any]) {
  def get(key: Symbol) = values(key)
}
