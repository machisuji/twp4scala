package twp4scala.tools

object Debugger {
  def inspect(in: twp4scala.Input, stop: (Int, Int) => Boolean = (_, read) => read == -1): Unit = {
    var read = 0
    println(in.toString + ":")
    Iterator.continually(in.read).takeWhile { byte =>
      read += 1
      !stop(read, byte)
    }.foreach(byte => print(" " + byte))
    println()
  }
}
