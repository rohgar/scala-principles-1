package example

import scala.annotation.tailrec

object Factorial {

  def factorial(x: Int): Int = {
    if (x == 0) 1 else (x * factorial(x - 1))
  }

  def tailRecursiveFactorial(x: Int): Int = {
    @tailrec
    def loop(acc: Int, x: Int): Int = if (x == 0) acc else loop(acc * x, (x - 1))
    loop(1, x)
  }

}