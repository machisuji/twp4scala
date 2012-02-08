import java.io.ByteArrayInputStream
import java.net.Socket
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec
import twp4scala.auth.{Signer, Certificate, Signature}
import twp4scala.protocol.echo._
import twp4scala.{Input, Twp, TcpServer}

class Authentication extends Spec with ShouldMatchers {
  describe("Verification") {
    it("should work for server/client communication") {
      val echoServer = new TcpServer {
        val port = 6666
        def handleClient(socket: Socket) {
          Twp(Echo(socket)) { server =>
            val error = server.authenticateWith("src/test/resources/cert.p12", Some("foobar"), false)
            error.foreach(err => println("Error: " + err.msg))
            error should not be ('defined)
            server.in match {
              case Request(text) => server ! Reply(text, text.filter(_.isLetter).size)
              case _ => fail("Received no Echo Request")
            }
          }.left.toOption should not be ('defined)
        }
      }
      echoServer.start()
      try {
        Twp(Echo("localhost", 6666)) { client =>
          val error = client.authenticateWith("src/test/resources/cert.p12", Some("foobar"))
          error.foreach(err => println("Error: " + err.msg))
          error should not be ('defined)
          client ! Request("hallo")
          client.in match {
            case Reply(text, letters) => letters should be (5)
            case _ => fail("Received no Echo Reply")
          }
        }.left.toOption should not be ('defined)
      } finally {
        echoServer.stop()
      }
    }

    it("should succeed for valid signatures") {
      val signer = new Signer("src/test/resources/cert.p12", Some("foobar"))
      val in = new Input(new ByteArrayInputStream(
        Request("test").out.init ++ Signature.from(Request("test"))(signer).out))
      in.lastCertificate = Some(signer.certificate)
      in match {
        case Request(text) => text should equal ("test")
        case _ => fail("no Request read")
      }
    }

    it("should fail for invalid signatures") {
      val signer = new Signer("src/test/resources/cert.p12", Some("foobar"))
      val in = new Input(new ByteArrayInputStream(
        Request("test").out.init ++ Signature.from(Request("wrong"))(signer).out))
      in.lastCertificate = Some(signer.certificate)
      evaluating {
        in match {
          case Request(text) => text should equal ("test")
          case _ => fail("no Request read")
        }
      } should produce[IllegalArgumentException]
    }
  }
}
