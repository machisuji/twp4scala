import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec
import twp4scala._
import twp4scala.tools.MemoryProtocol

class Extensions extends Spec with ShouldMatchers {
  describe("Extension Messages") {
    it("should be read along with normal messages") {
      val sig = new Signature(Array[Byte](1, 2, 3))
      val input = MemoryProtocol(ErrorMessage(42, "foobar").out.init ++ sig.out)
      input.in match {
        case ErrorMessage(tag, msg) => msg should equal ("foobar")
        case _ => fail("Could not read ErrorMessage")
      }
      input.in.lastExtension.flatMap(Signature.from).map(ext =>
        ext.data.toSeq should equal (sig.data.toSeq)) getOrElse fail("no extension read")
    }

    it("should be sendable") {
      val sig = new Signature(Array[Byte](1, 2, 3))
      val input = MemoryProtocol()

      input.send(ErrorMessage(42, "baz"), Some(sig))
      input.in match {
        case ErrorMessage(tag, msg) => msg should equal ("baz")
        case _ => fail("no ErrorMessage sent")
      }
      input.in.lastExtension.flatMap(Signature.from).map(ext =>
        ext.data.toSeq should equal (sig.data.toSeq)) getOrElse fail("no extension sent")
    }
  }
}
