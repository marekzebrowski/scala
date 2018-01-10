import scala.annotation.tailrec

class Accumulate {
  // stack safe accumulate - extra pass on reverse
  def accumulate[A, B](f: (A) => B, list : List[A]): List[B] = {

    @tailrec
    def acc0(l: List[A], r:List[B]):List[B] = {
      l match {
        case Nil    => r
        case h :: t => acc0(t, f(h) :: r)
      }
    }

    @tailrec
    def reverse(l: List[B], r:List[B]):List[B] = {
      l match {
        case Nil => r
        case h :: t => reverse(t, h :: r)
      }
    }

    reverse(acc0(list, Nil), Nil)
  }
}
