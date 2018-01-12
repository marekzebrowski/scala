
class Binary(s:String) {
  def toDecimal: Int = {
    if(s.forall(c => c == '0' || c == '1')) {
      s.foldLeft[Int](0) {
        case (acc, '0') => acc * 2
        case (acc, '1') => acc * 2 + 1
      }
    } else 0
  }
}

object Binary {
  def apply(s: String) = new Binary(s)
}