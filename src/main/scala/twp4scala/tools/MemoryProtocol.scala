package twp4scala.tools

import twp4scala.{Connection, Protocol}
import java.io.{OutputStream, ByteArrayOutputStream, InputStream, ByteArrayInputStream}


trait MemoryConnection extends Connection {
  def input: Array[Byte]
  def output = out.asInstanceOf[ByteArrayOutputStream].toByteArray
  def in: InputStream = new ByteArrayInputStream(input)
  val out: OutputStream = new ByteArrayOutputStream()
}

object MemoryProtocol {
  def apply(bytes: Array[Byte]): MemoryConnection with Protocol = new MemoryConnection with Protocol {
    def protocolId = -1
    def input = if (bytes != null) bytes else output

    def initiate = ()
    def close = ()
  }

  def apply(): MemoryConnection with Protocol = apply(null)
}
