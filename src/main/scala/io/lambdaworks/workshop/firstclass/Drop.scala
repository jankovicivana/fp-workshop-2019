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
    def inner(elements: List[A], result: List[A]): List[A] =
      if(elements.isEmpty) result
      else if (p(elements.head)) inner(elements.tail, result)
      else inner(elements.tail, result :+ elements.head)

    inner(elements, Nil)
  }

}
