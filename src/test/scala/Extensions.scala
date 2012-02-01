import java.io.ByteArrayInputStream
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec
import twp4scala._
import twp4scala.tools.DebugProtocol

class Extensions extends Spec with ShouldMatchers {
  describe("Extension Messages") {
    it("should be read along with normal messages") {
      val sig = new Signature(Array[Byte](1, 2, 3))
      val input = DebugProtocol(ErrorMessage(42, "foobar").out.init ++ sig.out)
      println("data: " + input.input.mkString(" "))
      input.in match {
        case ErrorMessage(tag, msg) => msg should equal ("foobar")
        case _ => fail("Could not read ErrorMessage")
      }
      input.lastExtension.map(ext =>
        ext.values.head.asInstanceOf[Array[_]].toSeq should equal (sig.data.toSeq)) getOrElse fail("no extension read")
    }
  }
}
