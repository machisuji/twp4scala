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

  def getCurrentStackTrace(number: Int, stackTraceOffset: Int = 0): Seq[StackTraceElement] = {
    require(stackTraceOffset >= 0)
    var e: Exception = null
    try { throw new Exception("log") } catch { case ex: Exception => e = ex }
    val st = e.getStackTrace
    val start = 1 + stackTraceOffset
    for {
      i <- start to (start + number - 1)
      line = st.apply(i + stackTraceOffset)
    } yield line
  }

  def getCurrentStackTraceElement(stackTraceOffset: Int = 0) = getCurrentStackTrace(1, stackTraceOffset + 1).head

  def getLines(number: Int, stackTraceOffset: Int = 0) = getCurrentStackTrace(number, stackTraceOffset + 1).map(ste =>
    ste.getFileName + ":" + ste.getLineNumber)

  def getLine(stackTraceOffset: Int = 0) = getLines(1, stackTraceOffset + 1).head

  def testGetLine {
    println("Tests getLine")
    println("Line: " + getLine())
  }
}
