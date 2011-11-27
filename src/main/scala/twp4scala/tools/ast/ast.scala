package twp4scala.tools.ast

import java.io.PrintStream


sealed trait Tree {
  /**
   * Returns lines of Scala code expressing this node in Scala.
   */
  def toScala: List[String] = toString :: Nil

  implicit def richStrList(list: List[String]) = new RichStrList(list)

  protected def indent(depth: Int)(line: String): String =
    if (line.isEmpty) line
    else new String(Iterator.fill(depth)(' ').toArray) + line

  def print(out: PrintStream) = toScala.foreach(out.println)

  class RichStrList(list: List[String]) {
    def prependFirst(str: String): List[String] = (str + list.head) :: list.tail
    def appendFirst(str: String): List[String] = (list.head + str) :: list.tail
    def appendLast(str: String): List[String] = list.init :+ (list.last + str)
  }
}

trait Readable {
  def toScalaRead: List[String]
}
trait Writable {
  def toScalaWrite: List[String]
}

sealed trait SpecificationElement extends Tree
sealed trait ProtocolElement extends Tree
sealed trait Type extends Tree with Readable with Writable {
  def toScalaWrite: List[String] = toString :: Nil
  def toScalaRead: List[String] = toString :: Nil
}
sealed trait PrimitiveType extends Type
sealed trait TypeDefinition extends ProtocolElement

case class Specification(val elements: List[SpecificationElement]) extends Tree {
  override def toScala = elements.flatMap("" :: _.toScala)
}
case class Protocol(val identifier: Identifier, val id: Int, val elements: List[ProtocolElement]) extends SpecificationElement {
  override def toScala = {
    "package twp4scala.protocol.%s {".format(identifier.value.toLowerCase) ::
    "  import twp4scala._" ::
    "  import java.io.{InputStream, OutputStream}" ::
    "" ::
    "  trait %s extends Protocol {".format(identifier.value) ::
    "    def protocolId = %d".format(id) ::
    "  }" ::
    "" ::
    "  sealed trait Message extends twp4scala.Message" :: Nil ++
    elements.flatMap("" :: _.toScala).map(indent(2) )++(
    "}" :: Nil )
  }
}
case class MessageDefinition(val identifier: Identifier, val number: Int, val fields: List[Field])
    extends SpecificationElement with ProtocolElement {
  val isExtension = !(0 to 7).contains(number)

  override def toScala = {
    val typeTuple = Some("(" + fields.map(_.toScala.head.split(":").last.trim).mkString(", ") + ")").map(t =>
      if (t contains ',') t else t.substring(1, t.size - 1)).get
    val readTuple = Some("(" + fields.map(_.toScalaRead.head).mkString(", ") + ")").map(t =>
      if (t contains ',') t else t.substring(1, t.size - 1)).get
    val applySig = "(" + fields.map(_.toScala.head.substring(4)).mkString(", ") + ")"
    val applyBody = "new " + identifier.value + "(" + fields.map(_.id).mkString(", ") + ")"

    "class %s(%s) extends Message {".format(identifier.value, fields.flatMap(_.toScala).mkString(", ")) ::
    "  def write: Stream[Array[Byte]] = {" ::
    "    message(%s.tag) #::".format(identifier.value) :: Nil ++
    fields.flatMap(field => field.toScalaWrite.appendLast(" #::")).map(indent(4) )++(
    "    Stream.empty" ::
    "  }" ::
    "}" :: Nil )++(
    "" ::
    "object %s extends MessageCompanion[%s] {".format(identifier.value, typeTuple) ::
    "  def tag = %d".format(number) ::
    "  def apply%s = %s".format(applySig, applyBody) ::
    "  def read(implicit in: InputStream) = %s".format(readTuple) ::
    "}" ::
    Nil )
  }
}

case class Field(val optional: Boolean, val `type`: Type, val identifier: Identifier) extends Tree with Writable with Readable {

  val id = snailToCamelCase(identifier.value)

  override def toScala = {
    val ts = if (optional) "Option[" + `type`.toScala.head + "]" else `type`.toScala.head
    "val %s: %s".format(id, ts) :: Nil
  }
  def toScalaWrite = {
    def call(id: String) = `type`.toScalaWrite.appendLast("(" + id + ")")
    if (optional) call("v").prependFirst(id + ".map(v => ").appendLast(").getOrElse(nop)")
    else call(id)
  }
  def toScalaRead = `type`.toScalaRead

  protected def snailToCamelCase(id: String) = "([\\w&&[^_]])_([\\w&&[^_]])".r.replaceAllIn(id,
      (m) => m.group(1) + m.group(2).toUpperCase)
}

case object IntType extends PrimitiveType {
  override def toScala = "Int" :: Nil
  override def toScalaRead = toScalaWrite
  override def toScalaWrite = "someInt" :: Nil
}
case object StringType extends PrimitiveType {
  override def toScala = "String" :: Nil
  override def toScalaRead = toScalaWrite
  override def toScalaWrite = "string" :: Nil
}
case object BinaryType extends PrimitiveType {
  override def toScala = "Array[Byte]" :: Nil
  override def toScalaRead = toScalaWrite
  override def toScalaWrite = "binary" :: Nil
}
case object AnyType extends PrimitiveType {
  override def toScala = "AnyRef" :: Nil
}

case class Identifier(val value: String) extends Type

case class AnyDefinedBy(val identifier: Identifier) extends Type

case class StructDefinition(val identifier: Identifier, number: Option[Int], val fields: List[Field])
  extends SpecificationElement with TypeDefinition
case class SequenceDefinition(val `type`: Type, val identifier: Identifier) extends TypeDefinition
case class UnionDefinition(val identifier: Identifier, val caseDefinitions: List[CaseDefinition]) extends TypeDefinition
case class CaseDefinition(val number: Int, `type`: Type, val identifier: Identifier)
case class ForwardDefinition(val identifier: Identifier) extends TypeDefinition