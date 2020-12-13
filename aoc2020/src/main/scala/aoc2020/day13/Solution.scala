package aoc2020.day13

import aoc2020.utils._

object Solution {

  def parseSchedule(it: Iterator[String]): (Long, List[Option[Long]]) = {
    val time = it.next().toLong
    val busses = it.next().split(',').view.map(_.toLongOption).toList
    (time, busses)
  }

  def findSoonestDeparture(time: Long, busses: List[Option[Long]]): Long = {
    def remainingTime(busId: Long): (Long, Long) = {
      val mult = Math.ceil(time.toDouble / busId).toLong
      (busId, mult * busId - time)
    }

    val (id, t) = busses
      .flatMap(identity(_))
      .map(remainingTime)
      .minBy(_._2)

    id * t
  }

  def ensureCoprime(nums: List[Option[Long]]): Boolean = {
    def gcd(a: Long, b: Long): Long =
      if (b == 0) a
      else gcd(b, a % b)
    def loop(next: List[Long]): Boolean =
      next match {
        case Nil => true
        case p::rest =>
          rest.forall(n => gcd(n, p) == 1L) &&
          loop(rest)
      }
    loop(nums.flatten)
  }

  /*
   * Use Chinese remainder theorem to find
   *  solution (mod \prod_{i=0}^{busses} busInterval_i)
   */
  def optimalTime(busses: List[Option[Long]]): Long = {
    require(ensureCoprime(busses))

    // a^-1 (mod b)
    def multInv(a: Long, b: Long): Long = {
      def loop(a: Long, b: Long, x0: Long, x1: Long): Long =
        if (a <= 1) x1
        else loop(b, a % b, x1 - (a / b) * x0, x0)
      if (b == 1) 1
      else {
        val x1 = loop(a, b, 0, 1)
        if (x1 < 0) x1 + b else x1
      }
    }

    def solve(coefs: List[(Long, Long)]): Long = {
      val prod = coefs.foldLeft(1L)((prod, p) => prod * p._2)
      val res = coefs.foldLeft(0L) { case (sum, (a, n)) =>
        val p = prod / n
        sum + a * multInv(p, n) * p
      }
      res % prod
    }

    /*
     * Pairs (a_i, p_i) where a_i = Bus Interval - index(seconds offset) -
     * - 1 modulo Bus Interval (since the firts bus otherwise would have 
     * a_1 = 0), and p_i is Bus Interval 
     */
    val coefs = busses.zipWithIndex.flatMap { case (v, idx) =>
      v.map(n => (Math.floorMod(n - idx - 1, n), n))
    }.toList

    solve(coefs) + 1
  }

  def main(args: Array[String]): Unit = {
    require(args.length == 1)

    val (time, busses) = getLines(args(0), parseSchedule)

    // Task 1
    println("Least lasting waiting: " + findSoonestDeparture(time, busses))

    // Task 2
    println("Perfect schedule time: " + optimalTime(busses))
  }

}
