package twp4scala

import tools.DebugInput
import tools.GayString._

trait AbstractProtocol extends Connection {
  def initiate
  def shutdown
}

trait Protocol extends AbstractProtocol with TwpReader with TwpWriter {

  def protocolId: Int

  /**
   * Can push back exactly one byte for checking message tags.
   */
  abstract override lazy val in: Input =
    if (!Twp.debug) new Input(super.in, 16)
    else new DebugInput(super.in, 16)

  def shutdown = close

  def send(msg: Message, ext: Option[CanBeExtension]) {
    if (Twp.debug) {
      Twp.debug = false
      val data = msg.write.map(_.mkString(" ")).mkString(" | ")
      println("[DEBUG] Sending message: " + msg)
      println("[DEBUG]                  " + "^(\\d+) \\|".r.replaceAllIn(data,
        m => paint(m.group(1), Magenta) + " |") + " | " + paint("0", Magenta))
      Twp.debug = true
      println("[debug] Sending " + msg.write.flatten.mkString(" "))
    }
    if (ext.exists(_.isExtensionLike)) {
      val data = msg.write.toIterator
      while (data.hasNext) {
        val bytes = data.next
        if (data.hasNext) out.write(bytes)
        else out.write(bytes.init)
      }
      ext.get.write.foreach(out.write)
    } else msg.write.foreach(out.write)
    out.flush
  }

  def ! (msg: Message) = send(msg, None)

  /**
   * Sends a message along with the last read extension if there is one.
   * The extension is consumed in the process.
   */
  def !! (msg: Message) {
    send(msg, in.lastExtension)
    in.lastExtension = None
  }
}

trait Client extends Protocol {
  def initiate {
    val init = "TWP3\n".getBytes("ISO-8859-1")

    out write init
    out write shortInt(protocolId)

    out.flush()
  }
}

trait Server extends Protocol {
  def initiate {
    val init = "TWP3\n".getBytes("ISO-8859-1").toSeq
    val intro = in.take(init.size).toSeq
    val pid = shortInt(in)

    if (init == intro) {
      if (pid != protocolId) {
        Console.err.println("Client talks unsupported protocol " + pid + ". Expected " + protocolId + ".")
        this ! ErrorMessage(-1, "Unsupported protocol (" + pid + ")")
      }
    } else {
      Console.err.println("Client doesn't speak TWP. Closing connection.")
      close
    }
  }
}
