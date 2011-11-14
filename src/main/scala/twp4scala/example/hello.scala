package twp4scala.example

import twp4scala._
  import protocol.Echo

object Hello extends App {
  val echo = new Echo with TcpConnection {
    def host = args.drop(1).headOption              getOrElse "www.dcl.hpi.uni-potsdam.de"
    def port = args.drop(2).headOption.map(_.toInt) getOrElse 80
  }
  val msg = args.headOption getOrElse "Hallo Welt!"

  Client.run(echo) { echo =>
    val (text, letters) = echo.echo(msg)
    println("\"%s\" contains %d letters.".format(text, letters))
  }
}