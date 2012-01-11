import java.io.{ByteArrayInputStream, ByteArrayOutputStream, DataOutputStream, DataInputStream}
import java.util.NoSuchElementException
import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers
import twp4scala._
import twp4scala.tools.ast.{ApplicationType}
import twp4scala.tools.TDL

class TWP extends Spec with ShouldMatchers {

  describe("TDL Identifier Guards") {
    val rename = new TDL(Map(), id => if (id == "double") Some("Float64") else Some(id))
    val refuse = new TDL(Map(), id => if (id == "double") None else Some(id))
    Some(rename).map(tdl => tdl.parseAll(tdl.field, "double value;")).map(_.get).foreach(field =>
      field.`type`.toString should include ("Float64"))
    val thrown = evaluating(refuse.parseAll(refuse.field, "double value;")) should produce[RuntimeException]
    thrown.getMessage should include ("identifier")
  }

  describe("ApplicationTypes") {

    object tcp {
      class Float64(val value: Double) extends AppType[Double] {
        def write = {
          val data = new java.io.ByteArrayOutputStream
          val out = new java.io.DataOutputStream(data)
          out.writeDouble(value); out.close
          write(Float64, data.toByteArray)
        }
      }
      object Float64 extends AppTypeCompanion[Float64, Double] {
        val tag = 160
        def apply(value: Double) = new Float64(value)
        def read(implicit in: twp4scala.Input): Double = read((in, size) =>
          new java.io.DataInputStream(in).readDouble)

        def getApplicationType = ApplicationType[Double](160, "Float64")
      }

      class Operation(val name: String, val value: Float64) extends Struct {
        def write = Operation.tag.raw #:: name #:: value #:: End
      }
      object Operation extends StructCompanion[Operation, (String, Float64)] {
        def apply(values: (String, Float64)) = new Operation(values._1, values._2)
        def read(implicit input: twp4scala.Input) = (in[String], in[Float64])
      }
    }

    it("should be successfully parsed in a field with a defined Application Type") {
      val tdl = new TDL(Map("double" -> tcp.Float64.getApplicationType))
      val result = tdl.parseAll(tdl.field, "double value;")
      result.getOrElse(println("Parsing ApplicationType: " + result))
      result.successful should be (true)
      result.get.`type`.toString should include ("Float64")
    }

    it("should marshal correctly") {
      import tcp._
      val op = Operation("add", Float64(42))
      val inVal = Some(op.write).map { data =>
        val input = new java.io.PushbackInputStream(new ByteArrayInputStream(data.toArray.flatten))
        input match {
          case Operation(name, value) => value
          case _ => fail("Could not read Application Type")
        }
      }.get
      op.value should equal (inVal)
    }

    it("should generate proper Code") {
      val tdl = new TDL(Map("double" -> tcp.Float64.getApplicationType))
      val result = tdl.parseFile(tdl.specification, "tdl/tcp.tdl")
      result.successful should be (true)
      val code = result.get.toScala.mkString("\n")
      code should include ("object tcp")
      code should include ("class Float64")
      code should include ("object Float64")
      // actually I should simply try to compile it, though I shy at the additional dependency (scala-compiler.jar)
      // println(code)
    }
  }

  def shouldParse(file: String, print: Boolean = false) {
    val result = TDL.parseFile(TDL.specification, file)
    result.getOrElse(println("Parsing "+file+": "+result))
    assert(result.successful, "did not parse")
    if (print) {
      result.get.print
    }
  }

  describe("TDL should parse") { // parsing does not include validation (undefined types etc.)
    it("echo")(shouldParse("tdl/echo.tdl"))
    it("fam")(shouldParse("tdl/fam.tdl"))
    it("tfs")(shouldParse("tdl/tfs-rpc.tdl"))
    it("tcp")(shouldParse("tdl/tcp.tdl"))
  }
}
