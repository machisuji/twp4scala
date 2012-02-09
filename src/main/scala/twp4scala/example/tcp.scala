package twp4scala.example

import twp4scala._
  import tools.GayString._
import twp4scala.protocol.tcp._
import java.net.{InetAddress, Socket}

package object tcp {
  implicit def calculate(term: Term) = new {
    def get: Double = Calculator.get(term)
  }

  trait Operation {
    var host: String = "localhost"
    var port: Int
    val server: Calculator

    def hostBytes = InetAddress.getByName(host).getAddress

    def apply(terms: Term*): Term = {
      val args = Seq(terms: _*)
      val expr = Expression(hostBytes, port, args)
      Term(expr)
    }
  }
  class Op(var port: Int, name: String, fun: (Seq[Double]) => Double) extends Operation {
    lazy val server = new Calculator(port, fun, name)
  }

  object Operation {
    val all = List(Add, Sub, Mul, Fak, Div, Pow, Sin, Cos, Tan, Pi, E, Sqrt, Neg)
  }

  object Add extends Op(9001, "Add", (values: Seq[Double]) => values.foldLeft(0d)((acc, v) => acc + v))
  object Mul extends Op(9002, "Mul", (values: Seq[Double]) => values.foldLeft(1d)((acc, v) => acc * v))

  object Fak extends Op(9003, "Fak", (values: Seq[Double]) =>
    (1l to values.head.asInstanceOf[Long]).map(_.toDouble).reduce(_ * _))

  object Sub extends Op(9004, "Sub", (values: Seq[Double]) => values.reduceLeft(_ - _))
  object Div extends Op(9005, "Div", (values: Seq[Double]) => values.reduce(_ / _))

  object Pow extends Op(9006, "Pow", (values: Seq[Double]) => scala.math.pow(values(0), values(1)))

  object Sin extends Op(9007, "Sin", (values: Seq[Double]) => scala.math.sin(values.head))
  object Cos extends Op(9008, "Cos", (values: Seq[Double]) => scala.math.cos(values.head))
  object Tan extends Op(9009, "Tan", (values: Seq[Double]) => scala.math.tan(values.head))

  object Pi extends Op(9010, "Pi", _ => scala.math.Pi)
  object E extends Op(9011, "E", _ => scala.math.E)

  object Sqrt extends Op(9012, "Sqrt", (values: Seq[Double]) => scala.math.sqrt(values.head))
  object Neg extends Op(9013, "Neg", (values: Seq[Double]) => values.head * -1d)

  object Calculator {
    def get(term: Term): Double = {
      term.value match {
        case value: Float64 => value.value
        case expr: Expression => {
          val result = Twp(TCP(InetAddress.getByAddress(expr.host).getHostAddress, expr.port)) { tcp =>
            val error = tcp.authenticateWith("mkahl.p12", Some("hallo133"))
            error.foreach(err => println("Auth failed: %s".format(err) painted Red))
            tcp ! Request(42, expr.arguments)
            tcp.in match {
              case Reply(rid, result: Float64) => result.value
              case Error(msg) => {
                throw new RuntimeException("Could not calculate term: " + msg)
              }
              case input => {
                twp4scala.tools.Debugger.inspect(input)
                throw new RuntimeException("Invalid response")
              }
            }
          }
          result.left.foreach {
            case e: java.net.ConnectException => println(
              "Could not connect to Operation servers (try Calculator.start).")
            case e => throw e
          }
          result.right.get
        }
      }
    }

    def start() = Operation.all.foreach(_.server.start)
    def stop() = Operation.all.foreach(_.server.stop)

    def host_=(host: String) = Operation.all.foreach(_.host = host)
    def host: Seq[String] = Operation.all.map(_.host)
  }

  class Calculator(val port: Int, val op: (Seq[Double]) => Double, name: String = null) extends TcpServer {

    def handleClient(socket: Socket) {
      Twp(TCP(socket)) { tcp =>
        try {
          val error = tcp.authenticateWith("mkahl.p12", Some("hallo133"), initiate = false)
          error.foreach(err => println("Auth failed: %s".format(err) painted Red))
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
                println("(" + (if (name != null) name else this.toString) + " " +
                  values.map(_.toString).mkString(", ") + ")")
                val finalResult: Either[Exception, Double] = try {
                  Right(op(values))
                } catch {
                  case e: Exception => Left(e)
                }
                finalResult.left.toOption.foreach(e =>
                  tcp ! Error(e.getClass + ": " + e.getMessage))
                finalResult.right.toOption.foreach(res =>
                  tcp ! Reply(rid, Float64(res)))
              }
            }
            case input => {
              println("TCP Server does not understand: ")
              twp4scala.tools.Debugger.inspect(input)
            }
          }
        } catch {
          case e: Exception => {
            println("Server: Could not handle Request")
            e.printStackTrace
            tcp ! Error(e.getClass + ": " + e.getMessage)
          }
        }
      }
    }

    def getResult(expr: Expression): Either[Error, Double] = {
      val host = InetAddress.getByAddress(expr.host).getHostAddress
      val client = TCP(host, expr.port)
      val result = Twp(client) { tcp =>
        val error = tcp.authenticateWith("mkahl.p12", Some("hallo133"))
        error.foreach(err => println("Auth failed: %s".format(err) painted Red))
        tcp ! Request(42, expr.arguments)
        tcp.in match {
          case Reply(rid: Int, value: Float64) => Right(value.value)
          case Error(text) => Left(Error(text))
        }
      }
      if (result.isLeft) Left(Error(result.left.get.getMessage))
      else result.right.get
    }

    override def connectionOpened(socket: java.net.Socket) = Unit
    override def connectionClosed(socket: java.net.Socket) = Unit
  }
}
