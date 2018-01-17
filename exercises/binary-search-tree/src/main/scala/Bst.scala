import Ordered._
case class Bst[T : Ordering](value: T, left: Option[Bst[T]], right: Option[Bst[T]]) {
  private def insertOpt(node:Option[Bst[T]], v: T): Bst[T] = {
    node match {
      case Some(n) => n.insert(v)
      case None => Bst(v, None, None)
    }
  }
  def insert(x: T): Bst[T] = {
    if(x < value) {
      Bst(value, Option(insertOpt(left, x)), right)
    } else if (x > value) {
      Bst(value, left, Option(insertOpt(right, x)))
    } else {
      this
    }
  }
}

object Bst {
  def fromList[T : Ordering](l: List[T]): Bst[T] = l match {
    case x::xs => xs.foldLeft(Bst(x, None, None))((r, e) => r.insert(e))
    case x::Nil => Bst(x, None, None)
    case Nil => throw new IllegalArgumentException("Tree must not be empty")
  }

  def toList[T](tree: Bst[T]): List[T] = toList(Some(tree))

  private def toList[T](tree: Option[Bst[T]]): List[T] = tree match {
    case Some(b) => toList(b.left) ++ List(b.value) ++ toList(b.right)
    case None => List.empty
  }

  def apply[T: Ordering](x: T): Bst[T] = Bst(x, None, None)
}