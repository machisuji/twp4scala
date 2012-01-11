import java.io.ByteArrayInputStream
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec
import twp4scala._
import twp4scala.{StructCompanion, Struct}

class Marshalling extends Spec with ShouldMatchers {

  type Args = Seq[Int]
  implicit object Args extends SeqCompanion[Args, Int]

  class Operation(val name: String, val args: Args) extends Struct {
    def write = Operation.tag.raw #:: name #:: args #:: End
  }
  object Operation extends StructCompanion[Operation, (String, Args)] {
    def apply(values: (String, Args)) = new Operation(values._1, values._2)
    def read(implicit input: twp4scala.Input) = (in[String], in[Args])
  }

  describe("Writing") {
    it("should work for arbitrary sequences (Seq)") {
      val seq = Seq(1, 2, 3)
      val converter = new TwpConversions {
        val data: Stream[Array[Byte]] = seq #:: Stream.empty
      }
      converter.data.head should equal (TwpConversions.writeSequence(seq)(twp4scala.shortIntWriter))
    }
  }

  describe("Reading") {
    it("should work for Structs (and by extension for Messages)") {
      val op = Operation("add", Seq(1, 2, 3))
      val readArgs = Some(op.write).map { data =>
        val input = new java.io.PushbackInputStream(new ByteArrayInputStream(data.toArray.flatten))
        input match {
          case Operation(name, args) => args
          case _ => fail("Could not read Application Type")
        }
      }.get
      op.args should equal (readArgs)
    }
  }
}
