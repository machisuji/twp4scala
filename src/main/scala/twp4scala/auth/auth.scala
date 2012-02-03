package twp4scala.auth

import twp4scala._

/**
 * @param data DER-encoded Certificate as defined in RFC 3280
 */
class Certificate(val data: Array[Byte]) extends Message {
  def write = Certificate.tag.msg #:: data.out #:: End
}

object Certificate extends MessageCompanion[Certificate, Array[Byte]] {
  def tag = 27
  def apply(data: Array[Byte]) = new Certificate(data)
  def read(implicit in: Input) = binary
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
  def tag = 28
  def apply(data: Array[Byte]) = new Signature(data)
  def read(implicit in: Input) = binary
}

class AuthError(val code: Int, val msg: String) extends Message {
  def write = AuthError.tag.msg #:: code.out #:: msg.out #:: End
}

object AuthError extends MessageCompanion[AuthError, (Int, String)] {
  def tag = 29
  def apply(values: (Int, String)) = new AuthError(values._1, values._2)
  def read(implicit input: Input) = (in[Int], in[String])
}
