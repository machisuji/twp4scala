package twp4scala

import auth.{Signature, Certificate}
import java.io.{InputStream, PushbackInputStream}

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

  def verify(data: Product) {
    import TwpConversions.{convert, writeAny}
    lastCertificate.foreach(cert =>
      lastExtension.flatMap(Signature from).foreach(sig =>
        require(cert.verify(data.productIterator.flatMap((field: Any) => field.out).toArray, sig.data),
          "Invalid Signature for: " + data.toString)))
  }

  def verify(date: Any): Unit = date match {
    case product: Product => verify(product)
    case any => verify(new Tuple1(any))
  }

  def this(in: InputStream) = this(in, 16)
}
