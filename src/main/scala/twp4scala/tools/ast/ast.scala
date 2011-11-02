package twp4scala.tools.ast

sealed trait SpecificationElement
sealed trait ProtocolElement
sealed trait Type
sealed trait PrimitiveType extends Type
sealed trait TypeDefinition extends ProtocolElement

case class Specification(val elements: List[SpecificationElement])
case class Protocol(val identifier: Identifier, val id: Int, val elements: List[ProtocolElement]) extends SpecificationElement
case class MessageDefinition(val identifier: Identifier, val number: Int, val fields: List[Field])
    extends SpecificationElement with ProtocolElement {
  val isExtension = !(0 to 7).contains(number)
}

case class Field(val optional: Boolean, val `type`: Type, val identifier: Identifier)

case object IntType extends PrimitiveType
case object StringType extends PrimitiveType
case object BinaryType extends PrimitiveType
case object AnyType extends PrimitiveType

case class Identifier(val value: String) extends Type

case class AnyDefinedBy(val identifier: Identifier) extends Type

case class StructDefinition(val identifier: Identifier, number: Option[Int], val fields: List[Field])
  extends SpecificationElement with TypeDefinition
case class SequenceDefinition(val `type`: Type, val identifier: Identifier) extends TypeDefinition
case class UnionDefinition(val identifier: Identifier, val caseDefinitions: List[CaseDefinition]) extends TypeDefinition
case class CaseDefinition(val number: Int, `type`: Type, val identifier: Identifier)
case class ForwardDefinition(val identifier: Identifier) extends TypeDefinition