package twp4scala.tools

import java.io.InputStream

class DebugInput(in: InputStream, bufferSize: Int = 1) extends twp4scala.Input(in, bufferSize) {
  override def read: Int = {
    print("[debug] \t\t\t\t|" + Debugger.getLines(5).mkString(" <- ") + "| reading ... ")
    try {
      val ret = super.read
      print(ret)
      ret
    } finally {
      println()
    }
  }

  override def unread(byte: Int) {
    println("[debug] \t\t\t\t|" + Debugger.getLines(5).mkString(" <- ") + "| unreading   " + byte)
    super.unread(byte)
  }
}
