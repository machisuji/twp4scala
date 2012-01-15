package twp4scala.example.tcp

import twp4scala._
import twp4scala.protocol.tcp._
import java.net.{InetAddress, Socket}

trait Operation {
  var host: String = "localhost"
  var port: Int

  def hostBytes = InetAddress.getByName(host).getAddress

  def apply(terms: Term*): Term = {
    val args = Seq(terms: _*)
    val expr = Expression(hostBytes, port, args)
    Term(expr)
  }
}
object Add extends Operation {
  var port = 9001
}
object Sub extends Operation {
  var port = 9002
}
object Mul extends Operation {
  var port = 9003
}

object Calculator {
  lazy val Adder = new Calculator(Add.port, (values: Seq[Double]) =>
    values.foldLeft(0d)((acc, v) => acc + v))
  lazy val Subtractor = new Calculator(Sub.port, (values: Seq[Double]) => {
    if (values.size == 0) 0d
    else if (values.size == 1) (-1d) * values.head
    else values.reduceLeft(_ - _)
  })
  lazy val Multiplier = new Calculator(Mul.port, (values: Seq[Double]) =>
    values.foldLeft(1d)((acc, v) => acc * v))

  def get(term: Term): Double = {
    term.value match {
      case value: Float64 => value.value
      case expr: Expression => {
        val result = Twp(TCP(InetAddress.getByAddress(expr.host).getHostAddress, expr.port)) { tcp =>
          tcp ! Request(42, expr.arguments)
          tcp.in match {
            case Reply(rid, result: Float64) => result.value
            case Error(msg) => throw new IllegalArgumentException(
              "Could not calculate term: " + msg)
            case input => {
              twp4scala.tools.Debugger.inspect(input)
              throw new RuntimeException("Connection aborted")
            }
          }
        }
        result.left.foreach(throw _)
        result.right.get
      }
    }
  }
}

class Calculator(val port: Int, val op: (Seq[Double]) => Double) extends TcpServer {

  def handleClient(socket: Socket) {
    val result = Twp(TCP(socket)) { tcp =>
      tcp.in match {
        case Request(rid: Int, params: Parameters) => {
          val values = params.flatMap { term =>
            term.value match {
              case value: Float64 => Some(value.value)
              case expr: Expression => {
                val result = getResult(expr)
                result.left.foreach(err => tcp ! err)
                result.right.toOption
              }
            }
          }
          if (values.size == params.size) { // no errors, yay
            val finalResult = op(values)
            tcp ! Reply(rid + 1, Float64(finalResult))
          }
        }
        case input => {
          println("TCP Server does not understand: ")
          twp4scala.tools.Debugger.inspect(input)
        }
      }
    }
    result.left.toOption.foreach { e =>
      println("Server: Could not handle Request")
      e.printStackTrace
    }
  }

  def getResult(expr: Expression): Either[Error, Double] = {
    val client = TCP(InetAddress.getByAddress(expr.host).getHostAddress, expr.port)
    val result = Twp(client) { tcp =>
      tcp ! Request(42, expr.arguments)
      tcp.in match {
        case Reply(rid: Int, value: Float64) => Right(value.value)
        case Error(text) => Left(Error(text))
      }
    }
    if (result.isLeft) Left(Error(result.left.get.getMessage))
    else result.right.get
  }
}
