package io.lambdaworks.workshop.firstclass

import scala.annotation.tailrec

object Drop {

  /**
    * Implement function that should drop elements that satisfy predicate function(p)
    * Implementation should pass suite DropFCFSpec
    */
  def dropIf[A](elements: List[A], p: A => Boolean): List[A] = {
    //elements.filterNot(p)

    @tailrec
    def inner(elements: List[A], result: List[A]): List[A] = elements match {
      case head :: tail if p(head) => inner(tail, result)
      case head :: tail => inner(tail, result :+ head)
      case Nil => result
    }

    inner(elements, Nil)
  }

}
