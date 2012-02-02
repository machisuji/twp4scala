import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec
import twp4scala.tools.{Debugger, MemoryProtocol}
import twp4scala._
import auth.Signature

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

    it("should be semi-implicitly sendable, too") {
      val ext = Any("Hallo", 704)
      val data = MemoryProtocol()

      data !! ErrorMessage(99, "breee")
      data.in match {
        case ErrorMessage(tag, _) => tag should equal (99)
        case _ => fail("no ErrorMessage sent")
      }
      data.in.lastExtension should not be ('defined)

      data.send(ErrorMessage(100, "bree"), Some(ext))
      data.in match {
        case ErrorMessage(_, msg) => msg should equal ("bree")
        case _ => fail("no ErrorMessage sent")
      }
      data.in.lastExtension should be ('defined)

      data !! ErrorMessage(1, "this is getting old") // should send along the lastExtension and consume it
      data.in.lastExtension should not be ('defined)
      data.in match {
        case ErrorMessage(tag, msg) => {
          tag should equal (1)
          msg should include ("old")
        }
        case _ => fail("no ErrorMessage sent")
      }
      data.in.lastExtension.map(_ should equal (ext)) getOrElse fail("no extension sent")
    }
  }
}
