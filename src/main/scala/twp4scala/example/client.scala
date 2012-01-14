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

object TfsClient {
  def forHPI = {
    import org.xbill.DNS._
    val records = new Lookup("tfs.dcl.hpi.uni-potsdam.de", Type.SRV).run
    records.headOption.map(_.asInstanceOf[SRVRecord]).map(srv =>
      TfsClient(srv.getTarget.toString, srv.getPort)
    ).getOrElse(throw new RuntimeException("DNS Lookup failed."))
  }
}

case class TfsClient(val host: String = "www.dcl.hpi.uni-potsdam.de", val port: Int = 80) {
  import protocol.tfs._

  val connection = new PersistentConnection[TFS] {
    def open = TFS(host, port)
  }

  /**
   * @param mode 0 - read | 1 - write | 2 - write (append)
   */
  def open(dir: Path, file: String, mode: Int) = connection.get { tfs =>
    tfs ! Request(1, "open", (dir, file, mode))
    tfs.in match {
      case Reply(rid, handle: Int) => handle.asInstanceOf[Long]
      case in => default(in)
    }
  }

  def close(handle: Long): Unit = connection.get (_ ! Request(0, "close", handle))

  def read(handle: Long, length: Int): Array[Byte] = connection.get { tfs =>
    tfs ! Request(1, "read", (handle, length))
    tfs.in match {
      case Reply(rid, data: Array[Byte]) => data
      case in => default(in)
    }
  }

  def write(handle: Long, data: Array[Byte]): Unit = connection.get { tfs =>
    tfs ! Request(0, "write", (handle, data))
  }

  def seek(handle: Long, offset: Long): Unit = connection.get { tfs =>
    tfs ! Request(0, "seek", (handle, offset))
  }

  def listdir(dir: Path): ListResult = connection.get { tfs =>
    tfs ! Request(1, "listdir", dir)
    tfs.in match {
      case Reply(rid, result: LooseStruct) => {
        val (directories, files) = result.get[Seq[String], Seq[String]]
        ListResult(directories, files)
      }
      case in => default(in)
    }
  }

  def stat(dir: Path, file: String): StatResult = connection.get { tfs =>
    tfs ! Request(1, "stat", StatParameters(dir, file))
    tfs.in match {
      case Reply(rid, result: LooseStruct) => {
        val (size, mtime, atime) = result.get[Int, Int, Int]
        StatResult(size, mtime, atime)
      }
      case in => default(in)
    }
  }

  def mkdir(dir: Path) = connection.get (_ ! Request(0, "mkdir", dir))
  def rmdir(dir: Path) = connection.get (_ ! Request(0, "rmdir", dir))

  def remove(dir: Path, file: String) = connection.get (_ ! Request(0, "remove", (dir, file)))

  def monitor(dir: Path, recursive: Boolean, host: String, port: Int): Long = connection.get { tfs =>
    val addr = java.net.InetAddress.getByName(host).getAddress
    val rec = if (recursive) 1 else 0
    tfs ! Request(1, "monitor", (dir, rec, addr, port.asInstanceOf[Long]))
    tfs.in match {
      case Reply(rid, handle: Int) => handle.asInstanceOf[Long]
      case in => default(in)
    }
  }

  def stopMonitoring(handle: Long): Unit = connection.get { tfs =>
    tfs ! Request(0, "stop_monitoring", handle)
  }

  protected def default[T](in: twp4scala.Input): T = {
    val error = in match {
      case ErrorMessage(msgType, error) => "Message (type " + msgType + ") failed: " + error
      case Tag(tag) => "Unexpected tag: " + tag
    }
    throw new IllegalStateException(error)
  }
}
