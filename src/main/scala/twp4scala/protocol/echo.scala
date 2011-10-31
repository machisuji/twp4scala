package twp4scala.protocol

import twp4scala._

trait Echo extends Twp {
  def echo(text: String): (String, Int) = {
    out write message(0) // msg 0 [=request]
    out write string(text)
    out write endOfContent

    val ret = message.map {
      case 1 => (string, shortInt) // msg 1 [=reply]
      case i => throw new RuntimeException("Expected Reply, got: " + i)
    }
    val end = in.read
    if (end != 0) throw new RuntimeException("Expected endOfContent, got: " + end)
    else ret.get
  }
}