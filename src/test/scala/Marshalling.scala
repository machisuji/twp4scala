import java.io.{PushbackInputStream, ByteArrayInputStream}
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec
import twp4scala.tools.{DebugProtocol, Debugger, DebugInput}
import twp4scala._
import twp4scala.{StructCompanion, Struct}

class Marshalling extends Spec with ShouldMatchers {

  type Args = Seq[Int]
  implicit object Args extends SeqCompanion[Args, Int]

  class Operation(val name: String, val args: Args) extends Struct {
    def write = Operation.tag.raw #:: name.out #:: args.out #:: End
  }
  object Operation extends StructCompanion[Operation, (String, Args)] {
    def apply(values: (String, Args)) = new Operation(values._1, values._2)
    def read(implicit input: twp4scala.Input) = (in[String], in[Args])
  }

  class Reply(val requestId: Int, val result: Any /* defined by requestId */) extends Message {
    def write = Reply.tag.msg #:: requestId.out #:: result.out #:: End
  }
  object Reply extends MessageCompanion[Reply, (Int, Any /* defined by requestId */)] {
    def tag = 1
    def apply(values: (Int, Any /* defined by requestId */)) = new Reply(values._1, values._2)
    def read(implicit in: Input) = (someInt, any)
  }

  describe("Writing") {
    it("should work for arbitrary sequences (Seq)") {
      val seq = Seq(1, 2, 3)
      val converter = new TwpConversions {
        val data: Array[Byte] = seq.out
      }
      converter.data should equal (TwpConversions.writeSequence(seq)(TwpConversions.writeShortInt).data)
    }
  }

  describe("Reading") {
    it("should work for Structs (and by extension for Messages)") {
      val op = Operation("add", Seq(1, 2, 3))
      val readArgs = Some(op.out).map { data =>
        val input = DebugProtocol(data)
        input.in match {
          case Operation(name, args) => args
          case _ => fail("Could not read Application Type")
        }
      }.get
      op.args should equal (readArgs)
    }

    it("should work for sequences (Seq)") { import TwpConversions._
      val seq = Seq(1, 2, 3)
      val data = seq.out
      implicit val input = new Input(new ByteArrayInputStream(data :+ 0.toByte))
      val result = TwpReader.in[Seq[Int]]

      seq should equal (result)
      input.available should be (1)
      input.read should equal (0)
    }
  }

  describe("Any") {
    it("should read Strings") { import TwpConversions._
      val str = "Hallo Welt!"
      implicit val input = new Input(new ByteArrayInputStream(str.out))
      val result = TwpReader.in[Any]

      result should equal (str)
    }

    it("should read structs") { import TwpConversions._
      val struct = ("Markus", 24, Seq("."))
      implicit val input = new Input(new ByteArrayInputStream(struct.out))
      val result = TwpReader.in[Any]

      result.isInstanceOf[LooseStruct] should be (true)
      val (name, age, path) = result.asInstanceOf[LooseStruct].get[String, Int, Seq[String]]
      name should equal  (struct._1)
      age should equal (struct._2)
      path should equal (struct._3)
    }

    it("should read structs with two consecutive sequences, too!") { import TwpConversions._
      val struct = (Seq("foo", "bar"), Seq("blah blah blah"))
      implicit val input = new Input(new ByteArrayInputStream(struct.out))
      val result = TwpReader.in[Any]

      result.isInstanceOf[LooseStruct] should be (true)
      val (s1, s2) = result.asInstanceOf[LooseStruct].get[Seq[String], Seq[String]]
      s1 should equal (struct._1)
      s2 should equal (struct._2)
    }

    it("should read messages, too") {
      val reply = Reply(3, (Seq("foo.txt", "bar.txt", "baz.txt"), Seq("var", "log", "lib")))
      val ls = Some(reply.write).map { data =>
        val input = new Input(new ByteArrayInputStream(data.toArray.flatten))
        input match {
          case Reply(name, ls: LooseStruct) => ls.get[Seq[String], Seq[String]]
          case _ => {
            Debugger.inspect(input)
            fail("Could not read Reply")
          }
        }
      }.get
      reply.result should equal (ls)
    }
  }
}
