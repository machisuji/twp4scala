package object twp4scala {
  type Input = java.io.PushbackInputStream
  val Output: Stream[Array[Byte]] = Stream.empty

  implicit object stringReader extends TwpReader with TwpReadable[String] with Preview {
    def read(implicit in: Input) = string
    def isDefinedAt(implicit in: Input) = check(tag => tag >= 17 && tag <= 127)
  }

  implicit object shortIntReader extends TwpReader with TwpReadable[Int] with Preview {
    def read(implicit in: Input) = shortInt
    def isDefinedAt(implicit in: Input) = check(13 ==)
  }

  implicit object longIntReader extends TwpReader with TwpReadable[Long] with Preview {
    def read(implicit in: Input) = longInt
    def isDefinedAt(implicit in: Input) = check(14 ==)
  }

  implicit object binaryReader extends TwpReader with TwpReadable[Array[Byte]] with Preview {
    def read(implicit in: Input) = binary
    def isDefinedAt(implicit in: Input) = check(tag => tag == 15 || tag == 16)
  }

  implicit object anyReader extends TwpReader with TwpReadable[Any] with Preview {
    def read(implicit in: Input) = Any.read
    def isDefinedAt(implicit in: Input) = Any.isDefinedAt
  }

  implicit def seqReader[T](implicit reader: TwpReadable[T]): TwpReadable[Seq[T]] =
    new SequenceReader[Seq[T], T] {
      def map(in: Input) = reader read in
      def canMap(in: Input) = reader.isDefinedAt(in)
    }

  implicit def productToStruct(struct: Product) = Any(struct.productIterator.toSeq)
}
