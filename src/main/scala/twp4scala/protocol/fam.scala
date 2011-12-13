package twp4scala.protocol

import twp4scala._
import java.net.Socket

package object fam extends TwpReader with TwpConversions {

  type Path = Seq[String]
  def path(implicit in: Input) = sequence[Path]

  trait FAM extends Protocol {
    def protocolId = 4
  }

  object FAM {
    def apply(h: String, p: Int) = new TcpConnection with FAM with Client {
      def host = h
      def port = p
    }

    def apply(s: Socket) = new SocketConnection with FAM with Server {
      val socket = s
    }
  }

  sealed trait Message extends twp4scala.Message // seal for pattern matching

  class Changed(val path: Path, val fileName: String) extends Message {
    def write = Changed.tag.msg #:: path #:: fileName #:: Output
  }
  object Changed extends MessageCompanion[Changed, (Path, String)] {
    def tag = 0
    def apply(values: (Path, String)) = new Changed(values._1, values._2)
    def read(implicit in: Input) = (path, string)
  }

  class Deleted(val path: Path, val fileName: String) extends Message {
    def write = Changed.tag.msg #:: path #:: fileName #:: Output
  }
  object Deleted extends MessageCompanion[Deleted, (Path, String)] {
    def tag = 1
    def apply(values: (Path, String)) = new Deleted(values._1, values._2)
    def read(implicit in: Input) = (path, string)
  }

  class Created(val path: Path, val fileName: String) extends Message {
    def write = Changed.tag.msg #:: path #:: fileName #:: Output
  }
  object Created extends MessageCompanion[Created, (Path, String)] {
    def tag = 0
    def apply(values: (Path, String)) = new Created(values._1, values._2)
    def read(implicit in: Input) = (path, string)
  }

  class StartExecuting(val path: Path, val fileName: String) extends Message {
    def write = Changed.tag.msg #:: path #:: fileName #:: Output
  }
  object StartExecuting extends MessageCompanion[StartExecuting, (Path, String)] {
    def tag = 0
    def apply(values: (Path, String)) = new StartExecuting(values._1, values._2)
    def read(implicit in: Input) = (path, string)
  }

  class StopExecuting(val path: Path, val fileName: String) extends Message {
    def write = Changed.tag.msg #:: path #:: fileName #:: Output
  }
  object StopExecuting extends MessageCompanion[StopExecuting, (Path, String)] {
    def tag = 0
    def apply(values: (Path, String)) = new StopExecuting(values._1, values._2)
    def read(implicit in: Input) = (path, string)
  }
  
  class MyPath(val value: Path, val fileName: String) extends Struct {
    def write = value #:: fileName #:: Output
  }
  object MyPath extends StructCompanion[MyPath, (Path, String)] {
    def apply(values: (Path, String)) = new MyPath(values._1, values._2)
    def read(implicit in: Input) = (path, string)
  }

  class Test(val path: MyPath, val fileName: String) extends Message {
    def write = Test.tag.msg #:: path #:: fileName #:: Output
  }
  object Test extends MessageCompanion[Test, (MyPath, String)] {
    def tag = 0
    def apply(values: (MyPath, String)) = new Test(values._1, values._2)
    def read(implicit in: Input) = (MyPath.in, string)
  }
  
  class Ping extends Message {
    def write = Ping.tag.msg #:: Output
  }
  object Ping extends Ping with EmptyMessageCompanion[Ping] {
    def tag = 0
  }

  class Reply(val requestId: Int, val result: Any) extends Message {
    def write = Reply.tag.msg #:: requestId #:: result #:: Output
  }
  object Reply extends MessageCompanion[Reply, (Int, Any)] {
    def tag = 1
    def apply(values: (Int, Any)) = new Reply(values._1, values._2)
    def read(implicit in: Input) = (someInt, any)
  }
}
