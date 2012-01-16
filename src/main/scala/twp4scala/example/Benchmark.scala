package twp4scala.example

import twp4scala.Twp
import twp4scala.protocol.echo.{Reply, Request, Echo}

/**
 * Benchmark testing twp4scala performance with:
 *   - 100000 EchoRequests mit Leerstring an einen lokalen Server
 *   -   1000 EchoRequests mit Leerstring an www.dcl.hpi.uni-potsdam.de
 *   -  10000 EchoRequests mit Strings von 10000 Leerzeichen an einen lokalen Server
 *            (Antwort: gleicher String, 0 als Zahl der Buchstaben, muss nicht berechnet werden.
 */
object Benchmark {
  def local100kEchoRequests() {
    val echoServer = new EchoServer(9000)
    echoServer.start()
    val ms = System.currentTimeMillis
    try {
      for (i <- 1 to 100000) {
        Twp(Echo("localhost", echoServer.port)) { echo =>
          echo ! Request("")
          echo.in match {
            case Reply(text, letters) => if (letters != 0) {
              println("empty echo request yielded invalid response: Reply(%s, %d)".format(text, letters))
            }
            case _ => println("empty echo request %d failed".format(i))
          }
        }.left.foreach { e =>
          println("100k local echo requests test failed: " + e.getClass + ": " + e.getMessage)
        }
      }
    } finally {
      println()
      val result = System.currentTimeMillis() - ms
      println("Time for 100k local, empty echo requests: " + result + " ms")
      echoServer.stop()
    }
  }

  def remote1kEchoRequests() {
    val ms = System.currentTimeMillis
    Twp(Echo("www.dcl.hpi.uni-potsdam.de", 80)) { echo =>
      for (i <- 1 to 1000) {
        echo ! Request("")
        echo.in match {
          case Reply(text, letters) => if (letters != 0) println("invalid echo response")
          case _ => throw new RuntimeException("Invalid response")
        }
      }
    }
    val result = System.currentTimeMillis - ms
    println("Time for 1000 remote echo requests: " + result + " ms")
  }

  def local10kEchoRequests() {
    val echoServer = new EchoServer(9000)
    val string = Iterator.fill(10000)(" ").mkString
    echoServer.start()
    val ms = System.currentTimeMillis
    try {
      for (i <- 1 to 10000) {
        Twp(Echo("localhost", echoServer.port)) { echo =>
          echo ! Request(string)
          echo.in match {
            case Reply(text, letters) => if (letters != 0) {
              println("empty echo request yielded invalid response: Reply(%s, %d)".format(text, letters))
            }
            case _ => println("empty echo request %d failed".format(i))
          }
        }.left.foreach { e =>
          println("100k local echo requests test failed: " + e.getClass + ": " + e.getMessage)
        }
      }
    } finally {
      println()
      val result = System.currentTimeMillis() - ms
      println("Time for 10k local echo requests: " + result + " ms")
      echoServer.stop()
    }
  }
}
