package twp4scala.example

import twp4scala._
import protocol.tfs._

object du extends App {
  val tfs = TfsClient()
  
  def size(file: String): Int = {
    val segs = file.split("/")
    val stat = tfs.stat(segs.init, segs.last)
    stat.size
  }
  
  args.map(file => file -> size(file)).foreach { case (file, size) =>
    println("%8d%s" format (size, file))
  }
}