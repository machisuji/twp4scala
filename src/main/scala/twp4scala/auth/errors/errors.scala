package twp4scala.auth.errors

import twp4scala.auth._

trait ErrorCompanion[T <: AuthError] {
  def code: Int
  def apply(msg: String): T
  def unapply(error: AuthError) = if (error.code == code) Some(apply(error.msg)) else None
}

class OtherError(msg: String) extends AuthError(OtherError.code, msg)
object OtherError extends ErrorCompanion[OtherError] {
  def code = 0
  def apply(msg: String) = new OtherError(msg)
}

class BadCertificate(msg: String) extends AuthError(BadCertificate.code, msg)
object BadCertificate extends ErrorCompanion[BadCertificate] {
  def code = 1
  def apply(msg: String) = new BadCertificate(msg)
}

class UnsupportedCertificate(msg: String) extends AuthError(UnsupportedCertificate.code, msg)
object UnsupportedCertificate extends ErrorCompanion[UnsupportedCertificate] {
  def code = 2
  def apply(msg: String) = new UnsupportedCertificate(msg)
}

class CertificateRevoked(msg: String) extends AuthError(CertificateRevoked.code, msg)
object CertificateRevoked extends ErrorCompanion[CertificateRevoked] {
  def code = 3
  def apply(msg: String) = new CertificateRevoked(msg)
}

class CertificateExpired(msg: String) extends AuthError(CertificateExpired.code, msg)
object CertificateExpired extends ErrorCompanion[CertificateExpired] {
  def code = 4
  def apply(msg: String) = new CertificateExpired(msg)
}

class CertificateUnknown(msg: String) extends AuthError(CertificateUnknown.code, msg)
object CertificateUnknown extends ErrorCompanion[CertificateUnknown] {
  def code = 5
  def apply(msg: String) = new CertificateUnknown(msg)
}

class IllegalParameter(msg: String) extends AuthError(IllegalParameter.code, msg)
object IllegalParameter extends ErrorCompanion[IllegalParameter] {
  def code = 6
  def apply(msg: String) = new IllegalParameter(msg)
}

class UnknownCertAuthority(msg: String) extends AuthError(UnknownCertAuthority.code, msg)
object UnknownCertAuthority extends ErrorCompanion[UnknownCertAuthority] {
  def code = 7
  def apply(msg: String) = new UnknownCertAuthority(msg)
}

class AccessDenied(msg: String) extends AuthError(AccessDenied.code, msg)
object AccessDenied extends ErrorCompanion[AccessDenied] {
  def code = 8
  def apply(msg: String) = new AccessDenied(msg)
}

class BadMacAddress(msg: String) extends AuthError(BadMacAddress.code, msg)
object BadMacAddress extends ErrorCompanion[BadMacAddress] {
  def code = 9
  def apply(msg: String) = new BadMacAddress(msg)
}

class CertificateNotYetValid(msg: String) extends AuthError(CertificateNotYetValid.code, msg)
object CertificateNotYetValid extends ErrorCompanion[CertificateNotYetValid] {
  def code = 10
  def apply(msg: String) = new CertificateNotYetValid(msg)
}
