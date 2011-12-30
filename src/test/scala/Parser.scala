import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers
import twp4scala.tools.ast.{ApplicationType}
import twp4scala.tools.TDL

class Parser extends Spec with ShouldMatchers {

  describe("ApplicationTypes") {
    it("should be successfully parsed in a field with a defined Application Type") {
      val doubleType = new ApplicationType[Double]("double") {
        def read(in: twp4scala.Input): Double = 42
        def write: Array[Byte] = Array[Byte]()
      }
      val tdl = new TDL(doubleType :: Nil)
      val result = tdl.parseAll(tdl.field, "double value;")
      result.getOrElse(println("Parsing ApplicationType: " + result))
      result.successful should be (true)
      result.get.`type` should be theSameInstanceAs doubleType
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