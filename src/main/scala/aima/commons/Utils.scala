package aima.commons

object Utils {

  //TODO: Can we make it lazy somehow
  def pairs[A](list: List[A]): List[(A,A)] = {

    def oLoop(list: List[A], result: List[(A,A)]): List[(A,A)] =
      list match {
        case x :: xs => oLoop(xs, iLoop(x,xs,Nil) ++ result)
        case Nil => result
      }

    def iLoop(elm: A, list: List[A], result: List[(A,A)]): List[(A,A)] =
      list match {
        case x :: xs => iLoop(elm, xs, (elm,x) :: result)
        case Nil => result
      }

    oLoop(list,Nil)
  }
}
