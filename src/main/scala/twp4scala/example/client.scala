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

  def listdir(dir: Path): ListResult = this get Twp(tfs) { tfs =>
    tfs ! Request(1, "listdir", dir)
    tfs.in match {
      case Reply(rid, result: TwpAny) => {
        val (directories, files) = result.get[Seq[String], Seq[String]]
        ListResult(directories, files)
      }
      case in => default(in)
    }
  }

  def stat(dir: Path, file: String): StatResult = this get Twp(tfs) { tfs =>
    tfs ! Request(1, "stat", StatParameters(dir, file))
    tfs.in match {
      case Reply(rid, result: TwpAny) => {
        val (size, mtime, atime) = result.get[Int, Int, Int]
        StatResult(size, mtime, atime)
      }
      case in => default(in)
    }
  }

  def mkdir(dir: Path): Unit = this get Twp(tfs) { tfs =>
    tfs ! Request(0, "mkdir", dir)
  }

  def rmdir(dir: Path): Unit = this get Twp(tfs) { tfs =>
    tfs ! Request(0, "rmdir", dir)
  }

  def remove(dir: Path, file: String): Unit = this get Twp(tfs) { tfs =>
    tfs ! Request(0, "remove", TwpAny(dir, file))
  }

  protected def get[S](twpResult: Either[Exception, S]): S = {
    val error = twpResult.left.toOption
    error map {err => println(err); null.asInstanceOf[S]} getOrElse twpResult.right.get
  }

  protected def default(in: twp4scala.Input) = in match {
    case Tag(msg) => throw new IllegalStateException("Unexpected tag " + msg)
  }
}
