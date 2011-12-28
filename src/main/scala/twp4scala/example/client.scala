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

case class TfsClient(val host: String = "www.dcl.hpi.uni-potsdam.de", val port: Int = 80) {
  import protocol.tfs._

  def tfs = TFS(host, port)

  /**
   * @param mode 0 - read | 1 - write | 2 - write (append)
   */
  def open(dir: Path, file: String, mode: Int) = perform { tfs =>
    tfs ! Request(1, "open", (dir, file, mode))
    tfs.in match {
      case Reply(rid, handle: Int) => handle.asInstanceOf[Long]
      case in => default(in)
    }
  }

  def close(handle: Long): Unit = perform (_ ! Request(0, "close", handle))

  def read(handle: Long, length: Int): Array[Byte] = this get Twp(tfs) { tfs =>
    tfs ! Request(1, "read", (handle, length))
    tfs.in match {
      case Reply(rid, data: Array[Byte]) => data
      case in => default(in)
    }
  }

  def write(handle: Long, data: Array[Byte]): Unit = perform { tfs =>
    tfs ! Request(0, "write", (handle, data))
  }

  def seek(handle: Long, offset: Long): Unit = perform { tfs =>
    tfs ! Request(0, "seek", (handle, offset))
  }

  def listdir(dir: Path): ListResult = perform { tfs =>
    tfs ! Request(1, "listdir", dir)
    tfs.in match {
      case Reply(rid, result: TwpAny) => {
        val (directories, files) = result.get[Seq[String], Seq[String]]
        ListResult(directories, files)
      }
      case in => default(in)
    }
  }

  def stat(dir: Path, file: String): StatResult = perform { tfs =>
    tfs ! Request(1, "stat", StatParameters(dir, file))
    tfs.in match {
      case Reply(rid, result: TwpAny) => {
        val (size, mtime, atime) = result.get[Int, Int, Int]
        StatResult(size, mtime, atime)
      }
      case in => default(in)
    }
  }

  def mkdir(dir: Path) = perform (_ ! Request(0, "mkdir", dir))
  def rmdir(dir: Path) = perform (_ ! Request(0, "rmdir", dir))

  def remove(dir: Path, file: String) = perform (_ ! Request(0, "remove", (dir, file)))

  def monitor(dir: Path, recursive: Boolean, host: String, port: Int): Long = perform { tfs =>
    val addr = java.net.InetAddress.getByName(host).getAddress
    val rec = if (recursive) 1 else 0
    tfs ! Request(1, "monitor", (dir, rec, addr, port.asInstanceOf[Long]))
    tfs.in match {
      case Reply(rid, handle: Int) => handle.asInstanceOf[Long]
      case in => default(in)
    }
  }

  def stopMonitoring(handle: Long): Unit = perform { tfs =>
    tfs ! Request(0, "stop_monitoring", handle)
  }

  protected def perform[S](io: (TFS => S)) = get(Twp(tfs)(io))

  protected def get[S](twpResult: Either[Exception, S]): S = {
    val error = twpResult.left.toOption
    error map { err =>
      if (Twp.debug) err.printStackTrace();
      else println(err.getMessage);
      null.asInstanceOf[S]} getOrElse twpResult.right.get
  }

  protected def default[T](in: twp4scala.Input): T = {
    val tag = in.read
    throw new IllegalStateException("Unexpected tag: " + tag)
  }
}
