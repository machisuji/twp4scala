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
  def timeout: Int = 30000

  val socket = {
    val s = new Socket(host, port)
    s.setSoTimeout(timeout)
    s
  }
  def in = socket.getInputStream
  def out = socket.getOutputStream
  def close = socket.close
}

trait SocketConnection extends Connection {
  val socket: Socket
  def in = socket.getInputStream
  def out = socket.getOutputStream
  def close = socket.close
}

/**
 * Can be used to perform Twp IO while automatically
 * reconnecting in case the connection is broken.
 */
abstract class PersistentConnection[T <: AbstractProtocol] {
  private var connection: Option[T] = None
  private var retryCount: Int = 0

  def open: T
  def close = {
    connection.foreach(_.shutdown)
    connection = None
  }

  def reconnect() {
    connection = Some(open)
    connection.foreach(_.initiate)
    Twp.log("Reconnecting " + this)
  }

  /**
   * Runs the given block using this connection.
   * Returns either an Exception, if one was thrown, or the actual result.
   */
  def apply[S](block: T => S): Either[Exception, S] = {
    if (!connection.isDefined) {
      reconnect()
    }
    try {
      Right(block(connection.get))
    } catch {
      case e: IOException if retryCount == 0 => { // prolly connection broken, try once again
        connection = None
        retryCount += 1
        apply(block)
      }
      case e: Exception => {
        retryCount = 0
        e.printStackTrace()
        Left(e)
      }
    }
  }

  def get[S](twpResult: Either[Exception, S]): S = {
    val error = twpResult.left.toOption
    error map { err =>
      if (Twp.debug) err.printStackTrace();
      else println(err.getMessage);
      null.asInstanceOf[S]} getOrElse twpResult.right.get
  }

  /**
   * While #apply will return either an Exception or the actual result,
   * get will return the result or null if an Exception was thrown.
   *
   * The Exception's error message will be printed to stdout.
   */
  def get[S](block: T => S): S = get(apply(block))
}
