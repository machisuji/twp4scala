package twp4scala.example

import twp4scala._
  import protocol.echo._

object Hello extends App {
  val echo = Echo (
    args.drop(1).headOption              getOrElse "www.dcl.hpi.uni-potsdam.de",
    args.drop(2).headOption.map(_.toInt) getOrElse 80
  )
  val msg = args.headOption getOrElse "Hallo Welt!"

  Runner.run(echo) { echo =>
    echo ! Request(msg)

    echo.in match {
      case Reply(text, letters) => println("'%s' contains %d letters" format (text, letters))
      case Tag(msg) => println("Unexpected response: tag " + msg)
    }
  }
}
