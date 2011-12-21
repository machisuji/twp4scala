package twp4scala

import java.lang.IllegalStateException


trait Struct extends Message {
  override val End = Stream(Array(0.toByte))
}

object Struct {
  val tag = 2
}

trait StructCompanion[S <: Struct, T] extends MessageCompanion[S, T] {
  def tag = Struct.tag
  override def isDefinedAt(implicit in: Input) =
    Some(message).filter(tag !=).map(in.unread).map(_ => false) getOrElse true
}

class TwpAny(val values: Seq[Any]) extends TwpWriter with TwpConversions with TwpWritable {
  val End = Stream(Array(0.toByte))

  def map(names: Symbol*) = {
    val ns: Seq[(Symbol, Any)] = names.toSeq.zip(values)
    new DynamicStruct(Map[Symbol, Any](ns: _*))
  }
  
  def write: Stream[Array[Byte]] = Array(2.toByte) #:: values.flatMap(writeAny).toArray #:: End
  
  override def toString = "TwpAny(" + values.toString() + ")"

  def get[T] = values.head.asInstanceOf[T]
  def get[T1, T2]: (T1, T2) = (values(0).asInstanceOf[T1], values(1).asInstanceOf[T2])
  def get[T1, T2, T3]: (T1, T2, T3) =
    (values(0).asInstanceOf[T1], values(1).asInstanceOf[T2], values(2).asInstanceOf[T3])
  def get[T1, T2, T3, T4]: (T1, T2, T3, T4) =
    (values(0).asInstanceOf[T1], values(1).asInstanceOf[T2], values(2).asInstanceOf[T3], values(3).asInstanceOf[T4])
  def get[T1, T2, T3, T4, T5]: (T1, T2, T3, T4, T5) =
    (values(0).asInstanceOf[T1], values(1).asInstanceOf[T2], values(2).asInstanceOf[T3],
      values(3).asInstanceOf[T4], values(4).asInstanceOf[T5])
}

object TwpAny extends TwpReader with TwpConversions with TwpReadable[Any] {
  
  def apply(values: Any*) = new TwpAny(values.toSeq)
  
  def read(implicit in: Input): Any = {
    val seqReader = new SeqReader
    val values = Iterator.continually(in.read).takeWhile(0 !=).flatMap { tag =>
      if (tag == 1) None
      else {
        if (tag != 2) in.unread(tag)
        Some(tag match {
          case 2 => TwpAny.read
          case 3 => sequence(in, seqReader) match {
            case seq: Seq[_] => {
              seq.head match {
                case any: TwpAny => any.values
                case any => throw new IllegalStateException("Unexpected seq head: " + any)
              }
            }
            case any => throw new IllegalStateException("Unexpected seq: " + any)
          }
          case 13 => someInt
          case 14 => someInt
          case 15 => binary
          case 16 => binary
          case n if n >= 17 && n <= 127 => string
          case n => throw new IllegalStateException("Unexpected tag in struct: " + n)
        })
      }
    }.toSeq
    if (values.size == 0) Unit
    else if (values.size == 1) {
      if (values.head.isInstanceOf[TwpAny]) in.unread(0)
      values.head
    }
    else new TwpAny(values)
  }
  
  class SeqReader extends SequenceReader[Any, Seq[Any]] {
    def map(in: Input) = TwpAny.read(in)
  }
}

case class DynamicStruct(val values: Map[Symbol, Any]) {
  def get(key: Symbol) = values(key)
}