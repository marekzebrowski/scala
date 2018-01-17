import scala.annotation.tailrec

object RunLengthEncoding {
  type Plain = String
  type Encoded = String


  def encode(str: Plain): String = {
    val end = str.length


    //acc should be StringBuilder really on jvm
    @tailrec
    def encodeChar(position: Int, prev: Option[Char], counter: Int, acc:String):String = {
      def encodePrev(c: Char) = {
        if (counter > 1) s"${counter}${c}"
        else s"${c}"
      }

      if(position >= end) {
        acc + prev.map(encodePrev).getOrElse("")
      } else {
        val c = str(position)
        prev match {
          case Some(prevChar) if (prevChar == c) =>
            encodeChar(position + 1, prev, counter + 1, acc)

          case Some(prevChar) =>
            val newAcc = acc + encodePrev(prevChar)
            encodeChar(position + 1, Some(c), 1, newAcc)

          case None => encodeChar(position + 1, Some(c), 1, acc)
        }
      }
    }

    encodeChar(0, None, 0, "")
  }

  //copied from example
  def decode(str: Encoded): Plain = {
    val nextChar: ((Int, Seq[Char]), Char) => (Int, Seq[Char]) = {
      case ((n, xs), x) =>
        if (x.isDigit) (n * 10 + x.asDigit, xs)
        else if (n == 0) (0, x +: xs)
        else (0, Seq.fill(n)(x) ++ xs)
    }

    val result = str.foldLeft((0, Seq.empty[Char]))(nextChar)
    result._2.reverse mkString
  }

}