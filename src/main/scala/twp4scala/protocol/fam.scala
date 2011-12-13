package twp4scala.protocol

import twp4scala._

object fam {

  trait FAM extends Protocol {
    def protocolId = 4
  }

  object FAM {
    def apply(h: String, p: Int) = new TcpConnection with FAM with Client {
      def host = h
      def port = p
    }

    def apply(s: java.net.Socket) = new SocketConnection with FAM with Server {
      val socket = s
    }
  }

  sealed trait Message extends twp4scala.Message

  type Path = Seq[String]
  object Path extends TwpReader with TwpConversions {
    def in(implicit in: Input) = sequence[Path]
  }

  class Changed(val directory: Path, val filename: String) extends Message {
    def write = Changed.tag.msg #:: directory #:: filename #:: End
  }
  object Changed extends MessageCompanion[Changed, (Path, String)] {
    def tag = 0
    def apply(values: (Path, String)) = new Changed(values._1, values._2)
    def read(implicit in: Input) = (Path.in, string)
  }

  class Deleted(val directory: Path, val filename: String) extends Message {
    def write = Deleted.tag.msg #:: directory #:: filename #:: End
  }
  object Deleted extends MessageCompanion[Deleted, (Path, String)] {
    def tag = 1
    def apply(values: (Path, String)) = new Deleted(values._1, values._2)
    def read(implicit in: Input) = (Path.in, string)
  }

  class Created(val directory: Path, val filename: String) extends Message {
    def write = Created.tag.msg #:: directory #:: filename #:: End
  }
  object Created extends MessageCompanion[Created, (Path, String)] {
    def tag = 2
    def apply(values: (Path, String)) = new Created(values._1, values._2)
    def read(implicit in: Input) = (Path.in, string)
  }

  class StartExecuting(val directory: Path, val filename: String) extends Message {
    def write = StartExecuting.tag.msg #:: directory #:: filename #:: End
  }
  object StartExecuting extends MessageCompanion[StartExecuting, (Path, String)] {
    def tag = 3
    def apply(values: (Path, String)) = new StartExecuting(values._1, values._2)
    def read(implicit in: Input) = (Path.in, string)
  }

  class StopExecuting(val directory: Path, val filename: String) extends Message {
    def write = StopExecuting.tag.msg #:: directory #:: filename #:: End
  }
  object StopExecuting extends MessageCompanion[StopExecuting, (Path, String)] {
    def tag = 4
    def apply(values: (Path, String)) = new StopExecuting(values._1, values._2)
    def read(implicit in: Input) = (Path.in, string)
  }
}
