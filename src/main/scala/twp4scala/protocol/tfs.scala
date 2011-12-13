package twp4scala.protocol

import twp4scala._

object tfs {

  trait TFS extends Protocol {
    def protocolId = 1
  }

  object TFS {
    def apply(h: String, p: Int) = new TcpConnection with TFS with Client {
      def host = h
      def port = p
    }

    def apply(s: java.net.Socket) = new SocketConnection with TFS with Server {
      val socket = s
    }
  }

  sealed trait Message extends twp4scala.Message

  type Path = Seq[String]
  object Path extends TwpReader with TwpConversions {
    def in(implicit in: Input) = sequence[Path]
  }

  type Filelist = Seq[String]
  object Filelist extends TwpReader with TwpConversions {
    def in(implicit in: Input) = sequence[Filelist]
  }

  class ListResult(val directories: Filelist, val files: Filelist) extends Struct {
    def write = ListResult.tag.msg #:: directories #:: files #:: End
  }
  object ListResult extends StructCompanion[ListResult, (Filelist, Filelist)] {
    def apply(values: (Filelist, Filelist)) = new ListResult(values._1, values._2)
    def read(implicit in: Input) = (Filelist.in, Filelist.in)
  }

  class StatResult(val size: Int, val mtime: Int, val atime: Int) extends Struct {
    def write = StatResult.tag.msg #:: size #:: mtime #:: atime #:: End
  }
  object StatResult extends StructCompanion[StatResult, (Int, Int, Int)] {
    def apply(values: (Int, Int, Int)) = new StatResult(values._1, values._2, values._3)
    def read(implicit in: Input) = (someInt, someInt, someInt)
  }

  class OpenParameters(val directory: Path, val file: String, val mode: Int) extends Struct {
    def write = OpenParameters.tag.msg #:: directory #:: file #:: mode #:: End
  }
  object OpenParameters extends StructCompanion[OpenParameters, (Path, String, Int)] {
    def apply(values: (Path, String, Int)) = new OpenParameters(values._1, values._2, values._3)
    def read(implicit in: Input) = (Path.in, string, someInt)
  }

  class ReadParameters(val fh: Int, val count: Int) extends Struct {
    def write = ReadParameters.tag.msg #:: fh #:: count #:: End
  }
  object ReadParameters extends StructCompanion[ReadParameters, (Int, Int)] {
    def apply(values: (Int, Int)) = new ReadParameters(values._1, values._2)
    def read(implicit in: Input) = (someInt, someInt)
  }

  class WriteParameters(val fh: Int, val data: Array[Byte]) extends Struct {
    def write = WriteParameters.tag.msg #:: fh #:: data #:: End
  }
  object WriteParameters extends StructCompanion[WriteParameters, (Int, Array[Byte])] {
    def apply(values: (Int, Array[Byte])) = new WriteParameters(values._1, values._2)
    def read(implicit in: Input) = (someInt, binary)
  }

  class SeekParameters(val fh: Int, val offset: Int) extends Struct {
    def write = SeekParameters.tag.msg #:: fh #:: offset #:: End
  }
  object SeekParameters extends StructCompanion[SeekParameters, (Int, Int)] {
    def apply(values: (Int, Int)) = new SeekParameters(values._1, values._2)
    def read(implicit in: Input) = (someInt, someInt)
  }

  class StatParameters(val directory: Path, val file: String) extends Struct {
    def write = StatParameters.tag.raw #:: directory #:: file #:: End
  }
  object StatParameters extends StructCompanion[StatParameters, (Path, String)] {
    def apply(values: (Path, String)) = new StatParameters(values._1, values._2)
    def read(implicit in: Input) = (Path.in, string)
  }

  class RemoveParameters(val directory: Path, val file: String) extends Struct {
    def write = RemoveParameters.tag.msg #:: directory #:: file #:: End
  }
  object RemoveParameters extends StructCompanion[RemoveParameters, (Path, String)] {
    def apply(values: (Path, String)) = new RemoveParameters(values._1, values._2)
    def read(implicit in: Input) = (Path.in, string)
  }

  class MonitorParameters(val directory: Path, val recursive: Int, val host: Array[Byte], val port: Int) extends Struct {
    def write = MonitorParameters.tag.msg #:: directory #:: recursive #:: host #:: port #:: End
  }
  object MonitorParameters extends StructCompanion[MonitorParameters, (Path, Int, Array[Byte], Int)] {
    def apply(values: (Path, Int, Array[Byte], Int)) = new MonitorParameters(values._1, values._2, values._3, values._4)
    def read(implicit in: Input) = (Path.in, someInt, binary, someInt)
  }

  class Request(val requestId: Int, val responseExpected: Int, val operation: String, val parameters: Any /* defined by operation */) extends Message {
    def write = Request.tag.msg #:: requestId #:: responseExpected #:: operation #:: parameters #:: End
  }
  object Request extends MessageCompanion[Request, (Int, Int, String, Any /* defined by operation */)] {
    def tag = 0
    def apply(values: (Int, Int, String, Any /* defined by operation */)) = new Request(values._1, values._2, values._3, values._4)
    def apply(responseExpected: Int, operation: String, parameters: Any) =
      new Request(nextRequestId, responseExpected, operation, parameters)
    def read(implicit in: Input) = (someInt, someInt, string, any)

    def nextRequestId = {
      requestId += 1
      requestId
    }
    private var requestId = 0
  }

  class Reply(val requestId: Int, val result: Any /* defined by requestId */) extends Message {
    def write = Reply.tag.msg #:: requestId #:: result #:: End
  }
  object Reply extends MessageCompanion[Reply, (Int, Any /* defined by requestId */)] {
    def tag = 1
    def apply(values: (Int, Any /* defined by requestId */)) = new Reply(values._1, values._2)
    def read(implicit in: Input) = (someInt, any)
  }

  class CancelRequest(val requestId: Int) extends Message {
    def write = CancelRequest.tag.msg #:: requestId #:: End
  }
  object CancelRequest extends MessageCompanion[CancelRequest, Int] {
    def tag = 2
    def apply(values: (Int)) = new CancelRequest(values)
    def read(implicit in: Input) = someInt
  }

  class CloseConnection() extends Message {
    def write = CloseConnection.tag.msg #:: End
  }
  object CloseConnection extends CloseConnection with EmptyMessageCompanion[CloseConnection] {
    def tag = 4
  }

  class RPCException(val text: String) extends Struct {
    def write = RPCException.tag.msg #:: text #:: End
  }
  object RPCException extends StructCompanion[RPCException, String] {
    def apply(values: (String)) = new RPCException(values)
    def read(implicit in: Input) = string
  }
}

