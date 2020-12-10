package aoc2020.day10

import aoc2020.utils._

object Solution {

  def readJolages(it: Iterator[String]): List[Int] =
    it.map(_.toInt).toList

  def countJoltDiffs(seq: List[Int]): Int = {
    def loop(seq: List[Int], ones: Int, threes: Int): Int =
      seq match {
        case Nil => ones * threes
        case List(x) if x == 3 => ones * (threes + 1)
        case List(x) if x == 1 => (ones + 1) * threes
        case x::y::tl if x - y == 3 =>
          loop(y::tl, ones, threes + 1)
        case x::y::tl if x - y == 1 =>
          loop(y::tl, ones + 1, threes)
        case _::tl =>
          loop(tl, ones, threes)
      }
    val sorted =
      seq.sorted(Ordering[Int].reverse) match {
        case Nil => Nil
        case h::tl => (h+3)::h::tl
      }
    loop(sorted, 0, 0)
  }

  def countArrangements(seq: List[Int]): Long = {
    def loop(
      seq: List[Int],
      prev1: (Int, Long),
      prev2: (Int, Long),
      prev3: (Int, Long),
    ): Long =
      seq match {
        case Nil => prev1._2
        case x::tl => {
          val sum = List(prev1, prev2, prev3)
            .foldLeft(0L) { case (acc, (y, sum)) =>
              if (x - y < 4) acc + sum
              else acc
            }
          loop(tl, (x, sum), prev1, prev2)
        }
      }
    val sorted =
      (0::seq).sorted(Ordering[Int].reverse) match {
        case Nil => Nil
        case h::tl => (h+3)::h::tl
      }
      
    loop(sorted.reverse, (0, 0), (0, 0), (0, 1))
  }

  def main(args: Array[String]): Unit = {
    require(args.length == 1)

    val joltages = getLines(args(0), readJolages)

    // Task 1
    println("Number of 1 and 3-jolt diffs: " + countJoltDiffs(joltages))

    // Task 2
    println("Arrangements: " + countArrangements(joltages))

  }

}
