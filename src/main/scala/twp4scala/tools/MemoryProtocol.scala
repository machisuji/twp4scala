package twp4scala.tools

import twp4scala.{Connection, Protocol}
import java.io._
import scala.collection.mutable.Queue

/**
 * Override data and/or in and out if you care for performance.
 */
abstract class MemoryConnection(initialData: Array[Byte]) extends Connection {

  val data: Queue[Byte] = Queue[Byte](initialData: _*)

  def in: InputStream = new InputStream {
    def read(): Int = if (data.isEmpty) -1 else data.dequeue() & 0xFF
  }
  def out: OutputStream = new OutputStream {
    def write(byte: Int): Unit = data.enqueue(byte.toByte)
  }
}

class MemoryProtocol(data: Array[Byte]) extends MemoryConnection(data) with Protocol {
  def protocolId = -1

  def initiate = ()
  def close = ()
}

object MemoryProtocol {
  def apply(data: Array[Byte]) = new MemoryProtocol(data)

  def apply(): MemoryProtocol = apply(Array[Byte]())
}
