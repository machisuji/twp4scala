package twp4scala.tools

import java.io.InputStream
import GayString._

class DebugInput(in: InputStream, bufferSize: Int = 16) extends twp4scala.Input(in, bufferSize) {
  override def read: Int = {
    val ret = super.read
    print(String.valueOf(ret) painted Magenta)
    print(" ")
    ret
  }

  override def read(data: Array[Byte], offset: Int, len: Int): Int = {
    val ret = super.read(data, offset, len)
    print(("  " + data.drop(offset).take(len).mkString(" ") + " | ") painted Magenta)
    ret
  }

  override def unread(byte: Int) {
    print(String.valueOf(byte) painted Blue)
    print(" ")
    super.unread(byte)
  }
}
