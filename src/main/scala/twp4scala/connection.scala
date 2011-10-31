package twp4scala

import java.io._
import java.net._

trait Connection {
  def in: InputStream
  def out: OutputStream
  def close: Unit
}

trait TcpConnection extends Connection {
  def host: String
  def port: Int

  val socket = new Socket(host, port)
  def in = socket.getInputStream
  def out = socket.getOutputStream
  def close = socket.close
}