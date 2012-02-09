package twp4scala

import auth.{Signature, Certificate}
import java.io.{InputStream, PushbackInputStream}
import collection.mutable.Queue
import tools.GayString._
import awesome.trailingConditionals

/**
 * The only class within which I need vars. :/
 */
class Input(
  in: InputStream,
  bufferSize: Int
) extends PushbackInputStream(in, bufferSize) {

  /**
   * Certificate used to verify incoming messages.
   */
  var lastCertificate: Option[Certificate] = None
  var lastExtension: Option[LooseStruct] = None // well, Input has state anyway, so what the heck

  private var verifier: java.security.Signature = null // I do know what I am doing
  private var doVerify = false
  private val verifyQueue: Queue[Int] = new Queue[Int]

  private def verifyLeft() = Iterator.continually(verifyQueue.nonEmpty).takeWhile(true ==).
    map(_ => verifyQueue.dequeue().toByte).foreach { byte =>
      print("%d ".format(byte) painted Green) provided Twp.debug
      verifier.update(byte)
    }

  private def verifyLater(byte: Int) = verifyQueue enqueue byte

  def startVerify() {
    if (lastCertificate.isDefined) {
      verifier = lastCertificate.map(_.createVerifier).get
      doVerify = true
    }
  }

  def stopVerify() {
    if (lastCertificate.isDefined) {
      doVerify = false
    }
  }

  def verify(): Boolean = {
    if (lastCertificate.isDefined) {
      verifyLeft()
      lastExtension.flatMap(Signature from).map(sig => verifier verify sig.data) getOrElse true
    } else true
  }

  override def read: Int = {
    val byte = super.read
    if (doVerify) {
      verifyLeft()
      verifyLater(byte)
    }
    byte
  }

  override def read(buffer: Array[Byte], offset: Int, length: Int): Int = {
    val read = super.read(buffer, offset, length)
    if (doVerify) {
      verifyLeft()
      if (length < 5) { // Axiom: The TWP implementation will never unread more than 4 bytes at a time
        for (i <- offset until length; byte = buffer(i).toByte) verifyLater(byte)
      } else {
        if (Twp.debug) {
          print("%s ".format(buffer.drop(offset).take(length).mkString(" ")) painted Green)
        }
        verifier.update(buffer, offset, length)
      }
    }
    read
  }

  override def unread(byte: Int) {
    if (doVerify) {
      verifyQueue.dequeueFirst(byte ==)
    }
    super.unread(byte)
  }

  override def unread(buffer: Array[Byte], offset: Int, length: Int) {
    if (doVerify) {
      buffer.foreach(byte => verifyQueue.dequeueFirst(byte ==))
    }
    super.unread(buffer, offset, length)
  }

  def checkIntegrity(data: Product) {
    import TwpConversions.{convert, writeAny}
    lastCertificate.foreach(cert =>
      lastExtension.flatMap(Signature from).foreach(sig =>
        require(cert.verify(data.productIterator.flatMap((field: Any) => field.out).toArray, sig.data),
          "Invalid Signature for: " + data.toString)))
  }

  def checkIntegrity(date: Any): Unit = date match {
    case product: Product => checkIntegrity(product)
    case any => checkIntegrity(new Tuple1(any))
  }

  def this(in: InputStream) = this(in, 16)
}
