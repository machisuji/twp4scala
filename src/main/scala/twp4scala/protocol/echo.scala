package twp4scala.protocol.echo

import twp4scala._
import java.io.{OutputStream, InputStream}
import java.net.Socket

trait Echo extends Protocol {
  def protocolId = 2
}

object Echo {
  def apply(h: String, p: Int) = new TcpConnection with Echo with Client {
    def host = h
    def port = p
  }

  def apply(s: Socket) = new SocketConnection with Echo with Server {
    val socket = s
  }
}

sealed trait Message extends twp4scala.Message // seal for pattern matching

class Request(val text: String) extends Message {
  def write: Stream[Array[Byte]] = {
    message(Request.tag) #::
    string(text) #:: Stream.empty
  }
}

class Reply(val text: String, val letters: Int) extends Message {
  def write: Stream[Array[Byte]] = {
    message(Reply.tag) #::
    string(text) #::
    someInt(letters) #:: Stream.empty
  }
}

object Request extends MessageCompanion[String] {
  def tag = 0
  def apply(text: String) = new Request(text)
  def read(implicit in: InputStream) = string
}

object Reply extends MessageCompanion[(String, Int)] {
  def tag = 1
  def apply(text: String, letters: Int) = new Reply(text, letters)
  def read(implicit in: InputStream) = (string, someInt)
}
