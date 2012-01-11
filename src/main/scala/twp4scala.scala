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

  implicit def seqReader[T](implicit reader: TwpReadable[T]): TwpReadable[Seq[T]] =
    new SequenceReader[Seq[T], T] {
      def map(in: Input) = reader read in
      def canMap(in: Input) = reader.isDefinedAt(in)
    }

  implicit def productToStruct(struct: Product) = new TwpAny(struct.productIterator.toSeq)

  implicit def shortIntWriter(i: Int) = new TwpWritable {
    def write: Stream[Array[Byte]] = Stream(TwpWriter.shortInt(i))
  }

  implicit def longIntWriter(l: Long) = new TwpWritable {
    def write: Stream[Array[Byte]] = Stream(TwpWriter.longInt(l.asInstanceOf[Int]))
  }

  implicit def stringWriter(str: String) = new TwpWritable {
    def write: Stream[Array[Byte]] = Stream(TwpWriter.string(str))
  }

  implicit def binaryWriter(bin: Array[Byte]) = new TwpWritable {
    def write: Stream[Array[Byte]] = Stream(TwpWriter.binary(bin))
  }

  implicit def seqWriter[T](values: Seq[T])(implicit writer: (T) => TwpWritable) = new TwpWritable with TwpWriter {
    def write: Stream[Array[Byte]] = Stream(
      sequence ++ values.map(writer).flatMap(_.write).flatten.toArray[Byte] ++ endOfContent)
  }

  implicit def writerToWriter(writer: TwpWritable) = writer

  implicit def anyWriter(any: Any) = new TwpWritable with TwpWriter {
    def write: Stream[Array[Byte]] = Stream(any match {
      case Raw(data) => data
      case a: TwpWritable => a.write.reduceLeft(_ ++ _)
      case i: Int => someInt(i)
      case l: Long => longInt(l.asInstanceOf[Int])
      case s: String => string(s)
      case s: Seq[_] => {
        if (!s.isEmpty) {
          val head = s.head
          head match {
            case e: String => seqWriter(s.asInstanceOf[Seq[String]]).write.flatten.toArray[Byte]
            case e: TwpWritable => sequence(s.asInstanceOf[Seq[TwpWritable]])
            case e => throw new IllegalStateException("Cannot write " + e + " of Seq " + s)
          }
        } else sequence(Nil)
      }
      case b: Array[Byte] => binary(b)
      case u: Unit => noValue
      case p: Product => new TwpAny(p.productIterator.toSeq).write.flatten.toArray
      case _ => throw new IllegalStateException("Cannot write " + any)
    })
  }
}
