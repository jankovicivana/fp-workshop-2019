package io.lambdaworks.workshop.recursion

import scala.annotation.tailrec

/**
  * Rewrite below non tail-recursive functions to tail-recursive one.
  * Add @tailrec annotation to prove it.
  */
object NonTail2TailRecursion {

  def factorial(n: Int): Int =
    if (n <= 0) 1 else n * factorial(n - 1)

  def factorialTail(n: Int): Int = {
    @tailrec
    def fact(n: Int, acc: Int): Int =
      if (n <= 0) acc else fact(n - 1, acc * n)

    fact(n, 1)
  }


  def cubesOfEvens(numbers: List[Double]): List[Double] =
    numbers match {
      case x :: xs if x % 2 == 0 => Math.pow(x, 3) +: cubesOfEvens(xs)
      case _ :: xs => cubesOfEvens(xs)
      case Nil     => List.empty
    }

  def cubesOfEvensTail(numbers: List[Double]): List[Double] = {
    @tailrec
    def inner(numbers: List[Double], acc: List[Double]): List[Double] =
      numbers match {
        case x :: xs if x % 2 == 0 => inner(xs, acc :+ Math.pow(x, 3))
        case _ :: xs => inner(xs, acc)
        case Nil     => List.empty
      }

    inner(numbers, List())
  }

}
