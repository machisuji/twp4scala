package twp4scala.tools.ast

import java.io.{FileWriter, PrintStream}


sealed trait Tree {
  /**
   * Returns lines of Scala code expressing this node in Scala.
   */
  def toScala: List[String] = toString :: Nil

  implicit def richStrList(list: List[String]) = new RichStrList(list)
  implicit def richerString(str: String) = new RicherString(str)

  protected def indent(depth: Int)(line: String): String =
    if (line.isEmpty) line
    else new String(Iterator.fill(depth)(' ').toArray) + line

  def print(out: PrintStream): Unit = toScala.foreach(out.println)
  def print(out: java.io.Writer) = {
    val pw = new java.io.PrintWriter(out)
    toScala.foreach(pw.println)
  }
  def print: Unit = print(System.out)
  
  def save(fileName: String) = print(new FileWriter(fileName))

  class RichStrList(list: List[String]) {
    def prependFirst(str: String): List[String] = (str + list.head) :: list.tail
    def appendFirst(str: String): List[String] = (list.head + str) :: list.tail
    def appendLast(str: String): List[String] = list.init :+ (list.last + str)
  }
  
  class RicherString(str: String) {
    def decapitalize = str.substring(0, 1).toLowerCase + str.substring(1)
  }

  protected def snailToCamelCase(id: String) = "([\\w&&[^_]])_([\\w&&[^_]])".r.replaceAllIn(id,
    (m) => m.group(1) + m.group(2).toUpperCase)
  
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
  def toScalaWrite: List[String] = "%s" :: Nil
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
    "" ::
    "  trait %s extends Protocol {".format(identifier.value) ::
    "    def protocolId = %d".format(id) ::
    "  }" ::
    "" ::
    "  sealed trait Message extends twp4scala.Message" :: Nil ++
    elements.flatMap("" :: _.toScala).map(indent(2)) ++ (
    "}" :: Nil)
  }
}

trait MessageSource { this: Tree =>
  val identifier: Identifier
  def tag: Option[Int]
  val fields: List[Field]
  val superClass: String // Message | Struct

  override def toScala = {
    val typeTuple = Some("(" + fields.map(_.toScala.head.split(":").last.trim).mkString(", ") + ")").map(t =>
      if (t contains ',') t else t.substring(1, t.size - 1)).get
    val readTuple = Some("(" + fields.map(_.toScalaRead.head).mkString(", ") + ")").map(t =>
      if (t contains ',') t else t.substring(1, t.size - 1)).get
    val applySig = "values: (" + fields.map { field =>
      val t = field.`type`.toScala.mkString
      if (field.optional) "Option[%s]".format(t)
      else t
    }.mkString(", ") + ")"
    val applyBody = "new " + identifier.value + "(" + fields.zipWithIndex.map { case (field, index) =>
      "values._" + (index + 1)
    }.mkString(", ") + ")"

    lazy val nonEmptyMessage = (
      "object %s extends %sCompanion[%s, %s] {".format(identifier.value, superClass, identifier.value, typeTuple) ::
        tag.map(n => "  def tag = %d".format(n)).getOrElse(skip) ::
        "  def apply(%s) = %s".format(if (fields.size > 0) applySig else "", applyBody) ::
        "  def read(implicit in: Input) = %s".format(readTuple) ::
        "}" ::
        Nil)

    lazy val emptyMessage = (
      "object %s extends Empty%sCompanion[%s] {".format(identifier.value, superClass, identifier.value) ::
        tag.map(n => "  def tag = %d".format(n)).getOrElse(skip) ::
        "}" ::
        Nil)

    "class %s(%s) extends %s {".format(identifier.value, fields.flatMap(_.toScala).mkString(", "), superClass) ::
      ("  def write = " +
        tag.map(_ => "%s.tag.msg #:: ".format(identifier.value)).getOrElse("") +
        fields.flatMap(field => field.toScalaWrite.appendLast(" #:: ")).mkString + "Output"
      ) :: "}" :: Nil ++ (if (fields.isEmpty) emptyMessage else nonEmptyMessage) filter (_ ne skip)
  }

  private val skip = new String
}

case class MessageDefinition(val identifier: Identifier, val number: Int, val fields: List[Field])
    extends SpecificationElement with ProtocolElement with MessageSource {

  val isExtension = !(0 to 7).contains(number)
  val superClass = "Message"
  def tag = Some(number)
}

case class Field(val optional: Boolean, val `type`: Type, val identifier: Identifier) extends Tree with Writable with Readable {

  val id = snailToCamelCase(identifier.value).decapitalize

  override def toScala = {
    val ts = if (optional) "Option[" + `type`.toScala.head + "]" else `type`.toScala.head
    "val %s: %s".format(id, ts) :: Nil
  }
  def toScalaWrite = {
    def call(id: String) = `type`.toScalaWrite.mkString.format(id)
    if (optional) "%s.map(%s).getOrElse(nop)".format(id, call(id)) :: Nil
    else call(id) :: Nil
  }
  def toScalaRead = `type`.toScalaRead
}

case object IntType extends PrimitiveType {
  override def toScala = "Int" :: Nil
  override def toScalaRead = "someInt" :: Nil
}
case object StringType extends PrimitiveType {
  override def toScala = "String" :: Nil
  override def toScalaRead = "string" :: Nil
}
case object BinaryType extends PrimitiveType {
  override def toScala = "Array[Byte]" :: Nil
  override def toScalaRead = "binary" :: Nil
}
case object AnyType extends PrimitiveType {
  override def toScala = "AnyRef" :: Nil
}

case class Identifier(aValue: String) extends Type {
  val value = snailToCamelCase(aValue).capitalize

  override def toScala = value :: Nil
  override def toScalaRead = value + ".in" :: Nil
}

case class AnyDefinedBy(val identifier: Identifier) extends Type

case class StructDefinition(val identifier: Identifier, val number: Option[Int], val fields: List[Field])
  extends SpecificationElement with TypeDefinition with MessageSource {

  val superClass = "Struct"
  def tag = number
}

case class SequenceDefinition(val `type`: Type, val identifier: Identifier) extends TypeDefinition {
  override def toScala = {
    val name = identifier.toScala.mkString
    val method = name.decapitalize
    "type %s = Seq[%s]".format(name, `type`.toScala.mkString) ::
    "def %s(implicit in: Input) = sequence[%s]".format(method, name) :: Nil
  }
}
case class UnionDefinition(val identifier: Identifier, val caseDefinitions: List[CaseDefinition]) extends TypeDefinition
case class CaseDefinition(val number: Int, `type`: Type, val identifier: Identifier)
case class ForwardDefinition(val identifier: Identifier) extends TypeDefinition