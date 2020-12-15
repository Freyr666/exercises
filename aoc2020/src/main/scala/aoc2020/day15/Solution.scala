package aoc2020.day15

import aoc2020.utils._

object Solution {

  def parseNumbers(s: String): (Int, Int, Map[Int, Int]) = {
    val nums = s
      .split(',')
      .flatMap(x => x.toIntOption)
    val env = nums
      .zip(nums.indices.dropRight(1))
      .foldLeft(Map.empty[Int, Int]) { case (m, (num, idx)) =>
        m.updated(num, idx)
      }
    (nums.last, nums.length - 1, env)
  }

  def findNth(n: Int, last: Int, lastIdx: Int, env: Map[Int, Int]): Long =
    if (lastIdx == n) last
    else env.get(last) match {
      case None =>
        findNth(n, 0, lastIdx + 1, env.updated(last, lastIdx))
      case Some(i) =>
        findNth(n, lastIdx - i, lastIdx + 1, env.updated(last, lastIdx))
    }

  def main(args: Array[String]) {
    require(args.length == 1)

    val (last, idx, env) = parseNumbers(args(0))

    // Task 1
    println("2020th number is " + findNth(2020-1, last, idx, env))

    // Task 2
    println("30000000th number is " + findNth(30000000-1, last, idx, env))
  }

}
