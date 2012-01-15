package twp4scala

import java.io.{Console => _, _}
import java.net._
import twp4scala._
import twp4scala.protocol.echo._

trait TcpServer extends Runnable {

  @volatile private var cancel = false

  val port: Int

  def handleClient(socket: Socket): Unit

  def run() {
    val socket = new ServerSocket(port)
    serverStarted(socket)
    try {
      while (!cancel) {
        val client = socket.accept
        val handler = new Runnable {
          def run {
            handleClient(client)
            connectionClosed(client)
          }
        }
        if (!cancel) {
          connectionOpened(client)
          new Thread(handler).start
        }
      }
    } catch {
      case e: Exception => {
        Console.err.println("Server crashed")
        e.printStackTrace()
      }
    } finally {
      serverStopped(socket)
      try { socket.close } catch { case _ => }
    }
  }

  def start() {
    cancel = false
    val server = new Thread(this)
    server.setDaemon(true)
    server.start
  }

  def stop() {
    cancel = true
    try {
      new Socket("localhost", port).close
    } catch {
      case e: Exception => println(e.getMessage)
    }
  }

  def serverStarted(server: ServerSocket) {
    println("Server listening on port " + port + " ...")
  }

  def serverStopped(server: ServerSocket) {
    println("Server stopped")
  }

  def connectionOpened(socket: Socket) {
    println("Request from " + socket.getInetAddress.getHostName + ":" + socket.getPort)
  }

  def connectionClosed(socket: Socket) {
    println("Closed connection to " + socket.getInetAddress.getHostName + ":" + socket.getPort)
  }
}
