import java.io.{ByteArrayInputStream, ByteArrayOutputStream, DataOutputStream, DataInputStream}
import java.util.NoSuchElementException
import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers
import twp4scala.tools.ast.{ApplicationType}
import twp4scala.tools.TDL
import twp4scala.{StructCompanion, Struct}

class TWP extends Spec with ShouldMatchers {

  describe("ApplicationTypes") {

    val doubleType = new ApplicationType[Double](160, "double") {
      def read(in: twp4scala.Input): Double = {
        new java.io.DataInputStream(in).readDouble
      }
      def write(value: Double): Array[Byte] = {
        val data = new java.io.ByteArrayOutputStream
        new java.io.DataOutputStream(data).writeDouble(value)
        data.toByteArray
      }
    }

    it("should be successfully parsed in a field with a defined Application Type") {
      val tdl = new TDL(doubleType :: Nil)
      val result = tdl.parseAll(tdl.field, "double value;")
      result.getOrElse(println("Parsing ApplicationType: " + result))
      result.successful should be (true)
      result.get.`type` should be theSameInstanceAs doubleType
    }

    it("should marshal correctly") {
      class Operation(val name: String, val value: Double) extends Struct {
        def write = Operation.tag.raw #:: name #:: ("double" @: value) #:: End
      }
      object Operation extends StructCompanion[Operation, (String, Double)] {
        def apply(values: (String, Double)) = new Operation(values._1, values._2)
        def read(implicit in: twp4scala.Input) = (string, @:[Double]("double"))
      }
      try {
        ApplicationType.register(doubleType)
        val op = Operation("add", 42)
        val inVal = Some(op.write).map { data =>
          val input = new java.io.PushbackInputStream(new ByteArrayInputStream(data.toArray.flatten))
          input match {
            case Operation(name, value) => value
            case _ => fail("Could not read Application Type")
          }
        }.get
        op.value should equal (inVal)
      } finally {
        ApplicationType.unregister(doubleType)
        evaluating(Operation("fail", 1).write.toArray.flatten) should produce [NoSuchElementException] // since double's missing
      }
    }
  }

  def shouldParse(file: String) {
    val result = TDL.parseFile(TDL.specification, file)
    result.getOrElse(println("Parsing "+file+": "+result))
    assert(result.successful, "did not parse")
  }

  describe("TDL should parse") { // parsing does not include validation (undefined types etc.)
    it("echo")(shouldParse("tdl/echo.tdl"))
    it("fam")(shouldParse("tdl/fam.tdl"))
    it("tfs")(shouldParse("tdl/tfs-rpc.tdl"))
    it("tcp")(shouldParse("tdl/tcp.tdl"))
  }
}