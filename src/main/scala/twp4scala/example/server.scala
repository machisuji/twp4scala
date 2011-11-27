package twp4scala.example

import java.io._
import java.net._
import twp4scala._
import twp4scala.protocol.echo._

class EchoServer(val port: Int) extends TcpServer {

  def handleClient(socket: Socket) {
    val echo = Echo(socket)
    val result = Runner.run(echo) { echo =>
      echo.in match {
        case Request(text)  => echo ! Reply(text, text.filter(_.isLetter).size)
        case Tag(msg)       => echo ! ErrorMessage(msg, "Unexpected message '" + msg + "', expected Request (0).")
      }
    }
    result.left.toOption.map(_.getMessage).foreach(println)
  }
}