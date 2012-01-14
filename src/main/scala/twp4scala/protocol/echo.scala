package twp4scala.protocol.echo

import twp4scala._
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
  def write = Request.tag.msg #:: text.out #:: Output
}

class Reply(val text: String, val letters: Int) extends Message {
  def write = Reply.tag.msg #:: text.out #:: letters.out #:: Output
}

object Request extends MessageCompanion[Request, String] {
  def tag = 0
  def apply(text: String) = new Request(text)
  def read(implicit in: Input) = string
}

object Reply extends MessageCompanion[Reply, (String, Int)] {
  def tag = 1
  def apply(values: (String, Int)) = new Reply(values._1, values._2)
  def read(implicit in: Input) = (string, someInt)
}
