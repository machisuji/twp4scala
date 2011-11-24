package twp4scala.protocol.echo

import twp4scala._
import java.io.{OutputStream, InputStream}

trait Echo extends Protocol[Echo] {
  def protocolId = 2
}

object Echo {
  def apply(h: String, p: Int) = new TcpConnection with Echo {
    def host = h
    def port = p
  }
}

sealed trait Message extends twp4scala.Message[Echo]

class Request(val text: String) extends Message {
  def write(out: OutputStream) {
    out write message(0)
    out write string(text)
  }
}

class Reply(val text: String, val letters: Int) extends Message {
  def write(out: OutputStream) {
    out write message(1)
    out write string(text)
    out write shortInt(letters)
  }
}

object Request extends MessageCompanion[Echo, Message, String] {
  def apply(text: String) = new Request(text)
  def isDefinedAt(implicit in: InputStream) = message == 0
  def read(implicit in: InputStream) = string
}

object Reply extends MessageCompanion[Echo, Message, (String, Int)] {
  def apply(text: String, letters: Int) = new Reply(text, letters)
  def isDefinedAt(implicit in: InputStream) = message == 1
  def read(implicit in: InputStream) = (string, int)
}
