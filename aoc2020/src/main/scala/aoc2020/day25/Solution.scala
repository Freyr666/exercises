package aoc2020.day25

import aoc2020.utils._

object Solution {

  // a^-1 (mod b)
  private def multInv(a: Long, b: Long): Long = {
      def loop(a: Long, b: Long, x0: Long, x1: Long): Long =
        if (a <= 1) x1
        else loop(b, a % b, x1 - (a / b) * x0, x0)
      if (b == 1) 1
      else {
        val x1 = loop(a, b, 0, 1)
        if (x1 < 0) x1 + b else x1
      }
    }

  def transform(num: Long, steps: Int, res: Long = 1L): Long =
    if (steps == 0) res
    else transform(num, steps-1, (res * num) % 20201227)

  def reverseTransfrom(num: Long): Int = {
    val sevenInv = multInv(7, 20201227)

    def loop(step: Int, next: Long): Int =
      if (next == 1) step
      else loop(step+1, (next * sevenInv) % 20201227)

    loop(0, num)
  }

  def main(args: Array[String]) = {
    require(args.length == 2)

    val cardPK = args(0).toLong
    val doorPK = args(1).toLong

    val cardSteps = reverseTransfrom(cardPK)
    val secret = transform(doorPK, cardSteps)
    println("The code is " + secret)

  }

}
