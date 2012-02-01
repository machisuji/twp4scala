package twp4scala

import java.io.{InputStream, PushbackInputStream}

class Input(in: InputStream, bufferSize: Int) extends PushbackInputStream(in, bufferSize) {
  var lastExtension: Option[LooseStruct] = None // well, Input has state anyway, so what the heck
  var lastCertificate: Option[Certificate] = None

  def this(in: InputStream) = this(in, 16)
}
