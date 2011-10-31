package twp4scala.example

import twp4scala._
  import protocol.Echo

object Hello extends App {
  val echo = new Echo with TcpConnection {
    def host = "www.dcl.hpi.uni-potsdam.de"
    def port = 80
  }

  Client.run(echo) { echo =>
    val (text, letters) = echo.echo("Hallo Welt!")
    println("\"%s\" contains %d letters.".format(text, letters))
  }
}