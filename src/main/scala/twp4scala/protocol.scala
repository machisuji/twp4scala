package twp4scala

import auth.errors.OtherError
import auth.{Signer, Signature, AuthError, Certificate}
import tools.GayString._
import java.io.OutputStream
import tools.{GayString, DebugInput}
import awesome.trailingConditionals

trait AbstractProtocol extends Connection {
  def initiate
  def shutdown
}

trait Protocol extends AbstractProtocol with TwpReader with TwpWriter { self =>

  def protocolId: Int

  abstract override lazy val in: Input =
    if (!Twp.debug) new Input(super.in, 16)
    else new DebugInput(super.in, 16)

  abstract override lazy val out: OutputStream = if (!Twp.debug) super.out else {
    val sout = super.out
    new java.io.OutputStream() {
      def write(byte: Int) = {
        sout.write(byte)
        print(GayString.paint(String.valueOf(byte), GayString.Cyan))
        print(" ")
      }

      override def flush() {
        sout.flush
        println("\n")
      }
    }
  }

  /**
   * Signer used to sign outgoing messages.
   */
  var signer: Option[Signer] = None

  def shutdown = close

  def send(msg: Message, ext: Option[CanBeExtension] = None) {
    if (Twp.debug) {
      Twp.debug = false
      val tag = "^(\\d+) \\|".r
      val eoc = "(\\d+)\\s*$".r
      val data = Some(msg.write.map(_.mkString(" ")).mkString(" | ")).map(str =>
        tag.replaceAllIn(str, m => paint(m.group(1), Magenta) + " |")).map(str =>
        eoc.replaceAllIn(str, m => paint(m.group(1), Magenta))).get
      println("[DEBUG] Sending message: " + msg)
      println("[DEBUG]                  " + data)
      Twp.debug = true
    }
    if (ext.exists(_.isExtensionLike)) {
      val data = msg.write.toIterator
      while (data.hasNext) {
        val bytes = data.next
        if (data.hasNext) out.write(bytes)
        else out.write(bytes.init)
      }
      ext.get.write.foreach(out.write)
      out.write(TwpWriter.endOfContent)
    } else msg.write.foreach(out.write)
    out.flush
  }

  /**
   * Sends the given message. Nothing more (read extensions), nothing less.
   * Use !! to send along the last read extension or #send to explicitly
   * send along a new extension.
   */
  def ! (msg: Message) = send(msg, signer.map(Signature from msg))

  /**
   * Sends a message along with the last read extension if there is one.
   * The extension is consumed in the process.
   */
  def !! (msg: Message) {
    send(msg, in.lastExtension)
    in.lastExtension = None
  }

  /**
   * Authenticate this connection.
   *
   * @param file Certificate to use (.p12 file for instance)
   * @param password Password if required
   * @param initiate Should we initiate or wait for the other side to start?
   *
   * @return Some authentication error if the authentication failed.
   */
  def authenticateWith(file: String, password: Option[String], initiate: Boolean = true): Option[AuthError] = {
    val signer = new Signer(file, password)
    send(signer.certificate) provided initiate
    in match {
      case AuthError(code, msg) => Some(AuthError(code, msg))
      case Certificate(cert) => {
        in.lastCertificate = Some(Certificate(cert)) // Server's certificate to check incoming messages
        self.signer = Some(signer)
        send(signer.certificate) unless initiate
        None
      }
      case _ => {
        val error = OtherError("Authentication failed (bad incoming certificate)")
        send(error)
        Some(error)
      }
    }
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
