package twp4scala

import java.io._

trait ByteOperations {
  implicit def richerInt(int: Int) = new RicherInt(int)
  implicit def richByteArray(bytes: Array[Byte]) = new RichByteArray(bytes)
  implicit def richInputStream(in: InputStream) = new RichInputStream(in)
}

class RicherInt(int: Int) {
  /** Yields the bytes of the given Int (up to <size>) big-endian. */
  def getBytes(size: Int = 4): Array[Byte] = (for {
    i <- 0 until Some(size).filter(1 <=).filter(4 >=).getOrElse(4)
    shift = i * 8
    byte = (int & (0xFF << shift)) >>> shift
  } yield byte.toByte).reverse.toArray
}

class RichByteArray(bytes: Array[Byte]) {
  def toInt = {
    val data = bytes.take(4)
    val parts = for {
      i <- 0 until data.size
      byte = (data(data.size - i - 1) & 0xFF) << (i * 8)
    } yield byte
    parts.foldLeft(0)((num, byte) => num | byte)
  }
}

class RichInputStream(in: InputStream) {
  def take(bytes: Int): Array[Byte] = {
    val data = new Array[Byte](bytes)
    val read = in.read(data)
    data.take(read)
  }
}