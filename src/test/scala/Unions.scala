import java.io.{PushbackInputStream, ByteArrayInputStream}
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec
import twp4scala.tools.DebugInput
import twp4scala.tools.Debugger
import twp4scala._

class Unions extends Spec with ShouldMatchers {

  type Path = Seq[String]
  object Path extends SeqCompanion[Path, String]

  case class File(value: Any) extends TwpWritable with TwpConversions {
    require(value.isInstanceOf[Path] || value.isInstanceOf[String])

    def write: Stream[Array[Byte]] = Stream(value match {
      case path: Path   => 4.toByte +: path.out
      case str: String  => 5.toByte +: str.out
    })

    def out = write.flatten.toArray[Byte]
  }

  implicit object File extends TwpReadable[File] with TwpReader {
    def isDefinedAt(implicit in: Input) =
      Preview.check(tag => (tag == 4 || tag == 5) && is[Path] || is[String])

    def read(implicit input: Input) = {
      val tg = input.read // throw away union tag
      File(
        if      (is[Path])    {in[Path]}
        else if (is[String])  {in[String]}
        else throw new RuntimeException("Unexpected input"))
    }
  }

  class Operation(val name: String, val value: File) extends Struct {
    def write = Operation.tag.raw #:: name.out #:: value.out #:: End
  }
  object Operation extends StructCompanion[Operation, (String, File)] {
    def apply(values: (String, File)) = new Operation(values._1, values._2)
    def read(implicit input: twp4scala.Input) = (in[String], in[File])
  }

  describe("Marshalling") {
    it("should work") { import TwpConversions._
      val op = new Operation("add", File(Seq(".")))
      implicit val input = new Input(new ByteArrayInputStream(op.out), 2) // Seqs need a pushback buffer size of 2+
      val (name, file) = input match {
        case Operation(name, file) => (name, file)
        case _ => fail("Result is no Operation")
      }
      name should equal (op.name)
      file should equal (op.value)
    }
  }
}
