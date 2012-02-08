package twp4scala

package object awesome {
  implicit def trailingConditionals[T](any: => T) = new {
    def provided(expr: Boolean) = if (expr) Some(any) else None
    def unless(expr: Boolean) = if (!expr) Some(any) else None
  }

  def maybe[T](value: T): Option[T] = if (value != null) Some(value) else None
}
