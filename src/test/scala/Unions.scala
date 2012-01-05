import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec
import twp4scala.{TwpReader, TwpConversions}

class Unions extends Spec with ShouldMatchers {

  type Path = Seq[String]
  object Path extends TwpReader with TwpConversions {
    def in(implicit in: twp4scala.Input) = sequence[Path]
  }

  trait File[T] // Union: Path | String
  object File {
    implicit object PathWitness extends File[Path]
    implicit object StringWitness extends File[String]
  }

  describe("Usage") {
    it("should (only) be possible to pass (otherwise unrelated) types defined in the union") {
      def read[T: File](value: T) = value match {
        case _: Seq[_] => "Path" // Path instead of Seq[_] would cause an erasure warning
        case _: String => "String"
      }
      read(Seq("/")) should equal ("Path")
      read("/etc/hosts") should equal ("String")
      // read(Seq(42)) and
      // read(false) and everything else without the expected union types won't compile, yay!
    }
  }
}
