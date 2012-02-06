package twp4scala

import auth.{Signature, Certificate}
import java.io.{InputStream, PushbackInputStream}

class Input(in: InputStream, bufferSize: Int) extends PushbackInputStream(in, bufferSize) {

  /**
   * Certificate used to verify incoming messages.
   */
  var lastCertificate: Option[Certificate] = None
  var lastExtension: Option[LooseStruct] = None // well, Input has state anyway, so what the heck

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
