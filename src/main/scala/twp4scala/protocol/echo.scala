package twp4scala.protocol

import twp4scala._

trait Echo extends Twp {

  def protocolId = 2

  def echo(text: String): (String, Int) = {
    out write message(0) // msg 0 [=request]
    out write string(text)
    out write endOfContent

    val result = message match {
      case 1 => (string, shortInt) // msg 1 [=reply]
      case n => throw new RuntimeException("Expected Reply, got: " + n)
    }
    expect(endOfContent); result
  }
}