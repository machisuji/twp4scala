package twp4scala

object Client {
  def run[T <: Protocol, S](proto: T)(block: T => S) = {
    proto.initiate
    try {
      block(proto)
    } finally {
      proto.shutdown
    }
  }
}