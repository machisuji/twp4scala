package twp4scala.auth

import twp4scala._
import java.security.cert.CertificateFactory
import java.io.{FileInputStream, InputStream, ByteArrayInputStream}
import java.security.{PrivateKey, KeyStore, Signature => JSignature}
import tools.GayString

/**
 * Signers are *not* thread-safe due to side effects of #sign.
 * Use one instance per Thread.
 */
class Signer(privateKeyFile: String, password: Option[String]) {
  val keyStore = KeyStore.getInstance(Signer.keyStoreType)

  keyStore.load(new FileInputStream(privateKeyFile), password.map(_.toCharArray) getOrElse null)

  protected val alias = {
    val aliases = keyStore.aliases
    require(aliases.hasMoreElements)
    aliases.nextElement
  }

  def certificate = Certificate(keyStore.getCertificate(alias))
  val key = keyStore.getKey(alias, password.map(_.toCharArray) getOrElse null).asInstanceOf[PrivateKey]
  val signature = JSignature.getInstance(Signature.algorithm)

  signature.initSign(key)

  def sign(data: Array[Byte]): Array[Byte] = {
    signature.update(data)
    signature.sign()
  }

  def sign(data: Array[Byte], offset: Int, length: Int) = {
    signature.update(data, offset, length)
    signature.sign()
  }
}
object Signer {
  val keyStoreType = "PKCS12"
}

class Certificate(val value: java.security.cert.Certificate) extends Message {
  def write = Certificate.tag.msg #:: value.getEncoded.out #:: End

  def verify(data: Array[Byte], signature: Array[Byte]): Boolean = {
    val sig = JSignature.getInstance(Signature.algorithm)
    sig.initVerify(value.getPublicKey)
    sig.update(data)
    sig.verify(signature)
  }

  /**
   * @return A java.security.Signature initialized for verifying through this Certificate.
   */
  def createVerifier = {
    val sig = JSignature.getInstance(Signature.algorithm)
    sig.initVerify(value.getPublicKey)
    sig
  }
}

object Certificate extends MessageCompanion[Certificate, java.security.cert.Certificate] {
  def tag = 27
  def apply(value: java.security.cert.Certificate) = new Certificate(value)
  def read(implicit in: Input) = decode(binary)

  def fromFile(fileName: String): Certificate = Certificate(decode(new FileInputStream(fileName)))

  /**
   * @param data DER-encoded Certificate as defined in RFC 3280
   */
  def decode(data: Array[Byte]): java.security.cert.Certificate = decode(new ByteArrayInputStream(data))

  def decode(input: InputStream): java.security.cert.Certificate = {
    val fact = CertificateFactory.getInstance("X.509")
    fact.generateCertificate(input)
  }
}

/**
 * Message and structure signing. Input to the signature are the
 * encoded TWP data of the fields of the message or struct up to
 * this extension value.
 *
 * @param data SHA1 with RSA encryption, algorithm 1.2.840.113549.1.1.5, as defined in RFC 2437
 */
class Signature(val data: Array[Byte]) extends Message {
  def write = Signature.tag.msg #:: data.out #:: End
}

object Signature extends MessageCompanion[Signature, Array[Byte]] {

  val algorithm = "SHA1withRSA"

  def tag = 28
  def apply(data: Array[Byte]) = new Signature(data)
  def read(implicit in: Input) = binary

  def from(msg: Message)(signer: Signer): Signature = {
    val data = msg.out
    Signature(signer.sign(data, 1, data.length - 2)) // drop msg/struct tag and end-of-content for signature
  }

  def sign(data: Array[Byte], signer: Signer): Array[Byte] = signer sign data
}

class AuthError(val code: Int, val msg: String) extends Message {
  def write = AuthError.tag.msg #:: code.out #:: msg.out #:: End

  override def toString = getClass.getSimpleName + " (" + code + "): " + msg
}

object AuthError extends MessageCompanion[AuthError, (Int, String)] {
  def tag = 29
  def apply(values: (Int, String)) = new AuthError(values._1, values._2)
  def read(implicit input: Input) = (in[Int], in[String])
}
