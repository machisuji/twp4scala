package twp4scala.example

import twp4scala._
  import protocol.echo._

object Hello extends App {
  val echo = Echo("www.dcl.hpi.uni-potsdam.de", 80)
  Client.run(echo) { echo =>
    echo ! Request("Let's fuck the world!")

    echo.receive {
      case Reply(text, letters) => println("'%s' contains %d letters" format (text, letters))
    }
  }
}