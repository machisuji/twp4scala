package twp4scala

import Twp.{log, logw}

trait TwpWriter extends ByteOperations {

  def tag(tagType: Int) = {
    log("tag(%d)" format tagType)
    Array(tagType.toByte)
  }

  def endOfContent = {
    log("endOfContent")
    Array(0.toByte)
  }
  def noValue = {
    log("noValue")
    Array(1.toByte)
  }

  def sequence: Array[Byte] = {
    log("sequence")
    Array(3.toByte)
  }

  def sequence[T <: TwpWritable](seq: Seq[T]): Array[Byte] = {
    logw(seq.toString)
    this.sequence ++ (seq.flatMap(_.write).flatten.toArray ++ endOfContent)
  }

  def message(id: Int): Array[Byte] = {
    if (id >= 0 && id <= 7) {
      logw("message(%d) (%d)" format (id,  id + 4))
      (4 + id).getBytes(1)
    } else { // send extension
      logw("extension(%d)" format id)
      12.getBytes(1) ++ id.getBytes()
    }
  }

  def shortInt(i: Int) = {
    logw("shortInt(%d)" format i)
    tag(13) ++ i.getBytes(1)
  }
  def longInt(i: Int) = {
    logw("longInt(%d)" format i)
    tag(14) ++ i.getBytes(4)
  }

  def someInt(i: Int) = if (i >= 256) longInt(i) else shortInt(i)

  def string(str: String): Array[Byte] = {
    logw("String(%s)" format str)
    val data = str.getBytes("UTF-8")
    val (msgTag, prefix) =
      if (data.size <= 109) tag(17 + data.size) -> Array[Byte]()
      else tag(127) -> data.size.getBytes()
    msgTag ++ prefix ++ data
  }

  def binary(data: Array[Byte]): Array[Byte] = {
    logw("binary(%s)" format data.toString)
    val (msgTag, prefix) =
      if (data.size <= 0xFF) tag(15) -> data.size.getBytes(1)
      else tag(16) -> data.size.getBytes(4)
    msgTag ++ prefix ++ data
  }

  def nop = {
    log("nop")
    new Array[Byte](0)
  }
}

object TwpWriter extends TwpWriter
