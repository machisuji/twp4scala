package twp4scala.protocol

import twp4scala._
import twp4scala.tools.Debugger

/**
 * The Calculator Protocol (TCP)
 */
package object tcp {

  trait TCP extends Protocol {
    def protocolId = 5
  }

  object TCP {
    def apply(h: String, p: Int) = new TcpConnection with TCP with Client {
      def host = h
      def port = p
    }

    def apply(s: java.net.Socket) = new SocketConnection with TCP with Server {
      val socket = s
    }
  }

  sealed trait Message extends twp4scala.Message

  /** Application Types */

  class Float64(val value: Double) extends AppType[Double] {
    def write: Stream[Array[Byte]] = {
      val data = new java.io.ByteArrayOutputStream
      val out = new java.io.DataOutputStream(data)
      out.writeDouble(value); out.close
      write(Float64, data.toByteArray)
    }
  }
  object Float64 extends AppTypeCompanion[Float64, Double] {
    def tag = 160
    def apply(values: (Double)) = new Float64(values)
    def read(implicit input: twp4scala.Input): Double = read((in, size) =>
      new java.io.DataInputStream(in).readDouble)
  }

  /** End Application Types */

  case class Term(value: Any) extends TwpWritable with TwpConversions {
    require(value.isInstanceOf[Float64] || value.isInstanceOf[Expression])

    def write: Stream[Array[Byte]] = Stream(value match {
      case value: Float64   => 4.toByte +: value.out
      case expr: Expression => 5.toByte +: expr.out
    })

    def out = write.flatten.toArray[Byte]
  }

  implicit object Term extends TwpReadable[Term] with TwpReader {
    def isDefinedAt(implicit in: Input) =
      Preview.check(tag => (tag == 4 || tag == 5) && is[Float64] || is[Expression])

    def read(implicit input: Input) = {
      val tg = input.read // throw away union tag
      Term(input match {
        case Float64(value)               => Float64(value)
        case Expression(host, port, args) => Expression(host, port, args)
        case _ => {
          println("Could not read Term")
          Debugger.inspect(input)
          throw new RuntimeException("Unexpected input while reading Term")
        }
      })
    }

    implicit def doubleToTerm(double: Double): Term = Term(Float64(double))
  }

  type Parameters = Seq[Term]
  object Parameters extends SeqCompanion[Parameters, Term]

  class Expression(val host: Array[Byte], val port: Int, val arguments: Parameters) extends Struct {
    def write = Expression.tag.raw #:: host.out #:: port.out #:: arguments.out #:: End
  }
  object Expression extends StructCompanion[Expression, (Array[Byte], Int, Parameters)] {
    def apply(values: (Array[Byte], Int, Parameters)) = new Expression(values._1, values._2, values._3)
    def read(implicit input: Input) = (in[Array[Byte]], in[Int], in[Parameters])
  }

  class Request(val requestId: Int, val arguments: Parameters) extends Message {
    def write = Request.tag.msg #:: requestId.out #:: arguments.out #:: End
  }
  object Request extends MessageCompanion[Request, (Int, Parameters)] {
    def tag = 0
    def apply(values: (Int, Parameters)) = new Request(values._1, values._2)
    def read(implicit input: Input) = (in[Int], in[Parameters])
  }

  class Reply(val requestId: Int, val result: Float64) extends Message {
    def write = Reply.tag.msg #:: requestId.out #:: result.out #:: End
  }
  object Reply extends MessageCompanion[Reply, (Int, Float64)] {
    def tag = 1
    def apply(values: (Int, Float64)) = new Reply(values._1, values._2)
    def read(implicit input: Input) = (in[Int], in[Float64])
  }

  class Error(val text: String) extends Message {
    def write = Error.tag.msg #:: text.out #:: End
  }
  object Error extends MessageCompanion[Error, String] {
    def tag = 2
    def apply(values: (String)) = new Error(values)
    def read(implicit input: Input) = in[String]
  }
}
