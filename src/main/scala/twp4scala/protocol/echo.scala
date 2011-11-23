package twp4scala.protocol

import twp4scala._
import java.io.PushbackInputStream

package echo {

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
    def send(p: Echo) { import p._
      out write message(0)
      out write string(text)
      out write endOfContent
    }
  }
  class Reply(val text: String, val letters: Int) extends Message {
    def send(p: Echo) { import p._
      out write message(1)
      out write string(text)
      out write shortInt(letters)
      out write endOfContent
    }
  }

  object Request extends MessageCompanion[Echo, Message] {
    def apply(text: String) = new Request(text)

    def unapply(input: PushbackInputStream): Option[String] = {
      val reader = new TwpReader with TwpWriter {
        def in = input
      }
      import reader._
      val tag = message
      if (tag == 0) {
        if (in.available() == 0) return Some(null)
        val res = new Request(string)
        expect(endOfContent)
        Some(res.text)
      } else {
        input unread message(tag)
        None
      }
    }
  }

  object Reply extends MessageCompanion[Echo, Message] {
    def apply(text: String, letters: Int) = new Reply(text, letters)

    def unapply(input: PushbackInputStream): Option[(String, Int)] = {
      val reader = new TwpReader with TwpWriter {
        val in = input
      }
      import reader._
      val tag = message
      if (tag == 1) {
        if (in.available() == 0) return Some(null)
        val res = new Reply(string, shortInt)
        expect(endOfContent)
        Some(res.text, res.letters)
      } else {
        input unread message(tag)
        None
      }
    }
  }
}
