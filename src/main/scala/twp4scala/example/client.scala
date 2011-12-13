package twp4scala.example

import twp4scala._
import java.lang.IllegalStateException

object Hello extends App {
  import protocol.echo._

  val echo = Echo (
    args.drop(1).headOption              getOrElse "www.dcl.hpi.uni-potsdam.de",
    args.drop(2).headOption.map(_.toInt) getOrElse 80
  )
  val msg = args.headOption getOrElse "Hallo Welt!"

  Twp(echo) { echo =>
    echo ! Request(msg)

    echo.in match {
      case Reply(text, letters) => println("'%s' contains %d letters" format (text, letters))
      case Tag(msg) => println("Unexpected response: tag " + msg)
    }
  }
}

case class TfsClient() {
  import protocol.tfs._

  def tfs = TFS("www.dcl.hpi.uni-potsdam.de", 80)

  def listdir(dir: Path): ListResult = Twp(tfs) { tfs =>
    tfs ! Request(1, "listdir", dir)
    tfs.in match {
      case Reply(rid, result: TwpAny) => {
        val directories = result.values(0).asInstanceOf[Seq[String]]
        val files = result.values(1).asInstanceOf[Seq[String]]
        ListResult(directories, files)
      }
      case Tag(msg) => throw new IllegalStateException("Unexpected " + msg)
    }
  }.right.get

  def stat(dir: Path, file: String): StatResult = Twp(tfs) { tfs =>
    tfs ! Request(1, "stat", StatParameters(dir, file))
    tfs.in match {
      case Reply(rid, result: TwpAny) => {
        val size = result.values(0).asInstanceOf[Int]
        val mtime = result.values(1).asInstanceOf[Int]
        val atime = result.values(2).asInstanceOf[Int]
        StatResult(size, mtime, atime)
      }
    }
  }.right.get
}
