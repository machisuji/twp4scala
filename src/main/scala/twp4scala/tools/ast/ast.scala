package twp4scala.tools.ast

import java.io.{FileWriter, PrintStream}
import twp4scala.Twp

object Tree {
  val skip = new String("// skip")
}

sealed trait Tree {
  /**
   * Returns lines of Scala code expressing this node in Scala.
   */
  def toScala: List[String] = toString :: Nil

  implicit def richStrList(list: List[String]) = new RichStrList(list)
  implicit def richerString(str: String) = new RicherString(str)

  protected def indent(depth: Int)(line: String): String =
    if (line.isEmpty || (line eq skip)) line
    else new String(Iterator.fill(depth)(' ').toArray) + line

  def print(out: PrintStream): Unit = toScala.foreach(out.println)
  def print(out: java.io.Writer) = {
    val pw = new java.io.PrintWriter(out)
    toScala.foreach(pw.println)
    pw.close
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
    def indentBy(depth: Int) = indent(depth)(str)
  }

  protected def snailToCamelCase(id: String) = "([\\w&&[^_]])_([\\w&&[^_]])".r.replaceAllIn(id,
    (m) => m.group(1) + m.group(2).toUpperCase)

  def skip = Tree.skip
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
  override def toScala = elements.flatMap("" :: _.toScala).filter(skip ne)
}
case class Protocol(val identifier: Identifier, val id: Int, val elements: List[ProtocolElement]) extends SpecificationElement {
  override def toScala = {
    "import twp4scala._" ::
    "" ::
    "object %s {".format(identifier.value.toLowerCase) ::
    "" ::
    "  trait %s extends Protocol {".format(identifier.value) ::
    "    def protocolId = %d".format(id) ::
    "  }" ::
    "" ::
    "  object %s {".format(identifier.value) ::
    "    def apply(h: String, p: Int) = new TcpConnection with %s with Client {".format(identifier.value) ::
    "      def host = h" ::
    "      def port = p" ::
    "    }" ::
    "" ::
    "    def apply(s: java.net.Socket) = new SocketConnection with %s with Server {".format(identifier.value) ::
    "      val socket = s" ::
    "    }" ::
    "  }" ::
    "" ::
    "  sealed trait Message extends twp4scala.Message" :: Nil ++
    elements.flatMap("" :: _.toScala).map(indent(2)) ++ (
    "}" :: Nil)
  }
}

trait MessageSource { this: Tree =>
  val identifier: Identifier
  def optTag: Option[Int]
  val fields: List[Field]
  val superClass: String // Message | Struct

  def writeMessage = ("def write = " +
    "%s.tag%s #:: ".format(identifier.value, if (superClass != "Struct") ".msg" else ".raw") +
    fields.flatMap(field => field.toScalaWrite.appendLast(" #:: ")).mkString + "End") :: Nil

  def readMessage = {
    val readTuple = Some("(" + fields.map(_.toScalaRead.head).mkString(", ") + ")").map(t =>
      if (t contains ',') t else t.substring(1, t.size - 1)).get
    "def read(implicit in: Input) = %s".format(readTuple) :: Nil
  }

  override def toScala = {
    val typeTuple = Some("(" + fields.map(_.toScala.head.split(":").last.trim).mkString(", ") + ")").map(t =>
      if (t contains ',') t else t.substring(1, t.size - 1)).get
    val applySig = "values: (" + fields.map { field =>
      val t = field.`type`.toScala.mkString
      if (field.optional) "Option[%s]".format(t)
      else t
    }.mkString(", ") + ")"
    val applyBody =
      if (fields.size == 1) "new %s(values)".format(identifier.value)
      else ("new " + identifier.value + "(" + fields.zipWithIndex.map { case (field, index) =>
        "values._" + (index + 1)
      }.mkString(", ") + ")")

    lazy val nonEmptyMessage = (
      "object %s extends %sCompanion[%s, %s] {".format(identifier.value, superClass, identifier.value, typeTuple) ::
        optTag.map(n => "  def tag = %d".format(n)).getOrElse(skip) ::
        "  def apply(%s) = %s".format(if (fields.size > 0) applySig else "", applyBody) :: Nil ++
        readMessage.map(indent(2)) ++
        ("}" :: Nil))

    lazy val emptyMessage = (
      "object %s extends %s with Empty%sCompanion[%s] {".format(identifier.value, identifier.value, superClass, identifier.value) ::
        optTag.map(n => "  def tag = %d".format(n)).getOrElse(skip) ::
        "}" ::
        Nil)

    ("class %s(%s) extends %s {".format(identifier.value, fields.flatMap(_.toScala).mkString(", "), superClass) :: Nil ++
    writeMessage.map(indent(2)) ++
    ("}" :: Nil) ++ (if (fields.isEmpty) emptyMessage else nonEmptyMessage)) filter (_ ne skip)
  }
}

case class MessageDefinition(val identifier: Identifier, val number: Int, val fields: List[Field])
    extends SpecificationElement with ProtocolElement with MessageSource {

  val isExtension = !(0 to 7).contains(number)
  val superClass = "Message"
  def optTag = Some(number)
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

/**
 * ApplicationType declaration.
 *
 * @param tag The TWP tag for this type (must be between 160 and 255 (incl.))
 * @param scalaTypeName The name of the generated Scala class
 */
case class ApplicationType[E](
  val tag: Int,
  val scalaTypeName: String
)(
  implicit ev$1: scala.reflect.Manifest[E]
) extends ProtocolElement with Type with MessageSource {

  require(tag >= 160 && tag <= 255, "ApplicationType tags must lie between 160 and 255 (incl.).")

  val enclosedTypeName = ev$1.erasure.getSimpleName.split("\\$").last.capitalize // capitalize primitives

  lazy val identifier = Identifier(scalaTypeName)
  val superClass = "AppType"
  lazy val optTag = Some(tag)
  lazy val fields = List(Field(false, Identifier(enclosedTypeName), Identifier("value")))

  override def readMessage =
    "def read(implicit in: twp4scala.Input): %s = read((in, size) =>".format(enclosedTypeName) ::
    "  throw new UnsupportedOperationException(\"Please implement me!\"))" :: Nil

  override def writeMessage =
    "def write = {" ::
    "  val data: Array[Byte] = {throw new UnsupportedOperationException(\"Please implement me!\")}" ::
    "  write(%s, data)".format(scalaTypeName) ::
    "}" :: Nil

  override def toString = "ApplicationType("+scalaTypeName+")"
}

object Identifier {
  val defaultIdGuard = (id: String) => Some(new Tree {
    val get = snailToCamelCase(id).capitalize
  }.get)
}

case class Identifier(
  tdlName: String,
  idGuard: (String) => Option[String] = Identifier.defaultIdGuard
) extends Type {
  val value = {
    val scalaId = idGuard(tdlName)
    if (Twp.debug) {
      scalaId.filter(tdlName !=).foreach(nid =>
        println("[INFO] Renaming TDL identifier %s to %s" format (tdlName, nid)))
    }
    scalaId.getOrElse(
      throw new RuntimeException("Invalid identifier: %s" format tdlName))
  }

  override def toScala = value :: Nil
  override def toScalaRead = value + ".in" :: Nil

  override def toString = {
    val name = if (tdlName == value) tdlName else (tdlName + " -> " + value)
    "Identifier(%s, %s)" format (name, idGuard.toString)
  }
}

case class AnyDefinedBy(val identifier: Identifier) extends Type {
  override def toScala = "Any /* defined by %s */".format(identifier.value.decapitalize) :: Nil

  override def toScalaRead = "any" :: Nil
}

case class StructDefinition(val identifier: Identifier, val number: Option[Int], val fields: List[Field])
  extends SpecificationElement with TypeDefinition with MessageSource {

  val superClass = "Struct"
  def optTag = number
}

case class SequenceDefinition(val `type`: Type, val identifier: Identifier) extends TypeDefinition {
  override def toScala = {
    val name = identifier.toScala.mkString
    "type %s = Seq[%s]".format(name, `type`.toScala.mkString) ::
    "object %s extends TwpReader with TwpConversions {".format(name) ::
    "  def in(implicit in: Input) = sequence[%s]".format(name) ::
    "}" :: Nil
  }
}
case class UnionDefinition(val identifier: Identifier, val caseDefinitions: List[CaseDefinition]) extends TypeDefinition
case class CaseDefinition(val number: Int, `type`: Type, val identifier: Identifier)
case class ForwardDefinition(val identifier: Identifier) extends TypeDefinition {
  override def toScala = skip :: Nil // don't need that at all
}

case class Comment(val text: String) extends Tree with ProtocolElement with SpecificationElement {
  override def toScala = List("/**" + text + "*/")
}
