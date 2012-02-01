package twp4scala.tools

import twp4scala.{Connection, Protocol}
import java.io.{OutputStream, ByteArrayOutputStream, InputStream, ByteArrayInputStream}


trait DebugConnection extends Connection {
  def input: Array[Byte]
  def output = out.asInstanceOf[ByteArrayOutputStream].toByteArray
  def in: InputStream = new ByteArrayInputStream(input)
  def out: OutputStream = new ByteArrayOutputStream()
}

object DebugProtocol {
  def apply(bytes: Array[Byte]) = new DebugConnection with Protocol {
    def protocolId = -1
    def input = bytes

    def initiate = ()
    def close = ()
  }
}
