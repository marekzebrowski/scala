import scala.annotation.tailrec

object BinarySearch {
  def find[T : Ordering](seq: Seq[T], value: T): Option[Int]
  = searchInternal(seq, value, 0, seq.size - 1)

  @tailrec
  def searchInternal[T : Ordering](seq: Seq[T], value: T,
                        start: Int, end: Int): Option[Int] = {
    val ord = implicitly[Ordering[T]]

    if (end < start || start < 0)
      None
    else {
      val middle = (start + end) / 2
      val elem = seq(middle)
      if (elem == value)
        Some(middle)
      else if (ord.compare(value, elem) < 0)
        searchInternal(seq, value, start, middle - 1)
      else
        searchInternal(seq, value, middle + 1, end)
    }
  }
}
