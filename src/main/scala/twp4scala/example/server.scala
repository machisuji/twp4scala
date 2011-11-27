package twp4scala.example

import java.io._
import java.net._
import twp4scala._
import twp4scala.protocol.echo._

class EchoServer(val port: Int) extends Runnable {

  @volatile var cancel = false

  def run() {
    val socket = new ServerSocket(port)
    println("EchoServer listening on port " + port + " ...")
    try {
      while (!cancel) {
        val client = socket.accept
        val whois = client.getInetAddress.getHostName + ":" + client.getPort
        val handler = new Runnable {
          def run {
            val echo = Echo(client)
            val result = Runner.run(echo) { echo =>
              echo.in match {
                case Request(text)  => echo ! Reply(text, text.filter(_.isLetter).size)
                case Tag(msg)       => echo ! ErrorMessage(msg, "Unexpected message '" + msg + "', expected Request (0).")
              }
            }
            result.left.toOption.map(_.getMessage).foreach(println)
            println("Closed connection to " + whois)
          }
        }
        if (!cancel) {
          println("Request from " + whois)
          new Thread(handler).start
        }
      }
    } finally {
      socket.close
      println("EchoServer stopped")
    }
  }

  def start() {
    cancel = false
    val server = new Thread(this)
    server.setDaemon(true)
    server.start
  }

  def stop() {
    cancel = true
    try {
      new Socket("localhost", port).close
    } catch {
      case e: Exception => println(e.getMessage)
    }
  }
}