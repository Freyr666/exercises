package aoc2020.day9

import aoc2020.utils._

object Solution {

  final class CodeQueue(
    val size: Int,
    ptr: Int,
    buffer: Vector[Long],
  ) {

    def add(code: Long): CodeQueue =
      new CodeQueue(
        size,
        (ptr + 1) % size,
        buffer.updated(ptr, code)
      )

    lazy val iterator: Iterator[Long] =
      buffer.iterator

    lazy val set: Set[Long] =
      buffer.toSet
  }

  object CodeQueue {
    def apply(size: Int): CodeQueue = {
      require(size > 0)
      new CodeQueue(size, 0, Vector.fill(size)(0))
    }
  }

  def findNotSum(seq: List[Long], size: Int = 25): Option[Long] = {
    def isSum(n: Long, queue: CodeQueue): Boolean =
      queue.iterator.exists { num =>
        queue.set(n - num)
      }

    def fillQueue(
      seq: List[Long],
      n: Int,
      queue: CodeQueue
    ): Option[(List[Long], CodeQueue)] =
      if (n == queue.size) Some((seq, queue))
      else seq match {
        case Nil => None
        case h::tl =>
          fillQueue(tl, n+1, queue.add(h))
      }

    def loop(
      seq: List[Long],
      queue: CodeQueue
    ): Option[Long] =
      seq match {
        case Nil => None
        case h::tl if ! isSum(h, queue) => Some(h)
        case h::tl => loop(tl, queue.add(h))
      }

    fillQueue(seq, 0, CodeQueue(size)).flatMap {
      case (list, queue) => loop(list, queue)
    }
  }

  def findContiguousSum(num: Long, seq: List[Long]): Option[Long] = {

    val vec = seq.reverse.toVector

    def check(start: Int, n: Int, acc: Long): Option[Long] = {
      val sum = acc + vec(n)
      if (sum > num) None
      else if (sum < num) check(start, n+1, sum)
      else {
        val range = vec.slice(start, n+1)
        Some(range.max + range.min)
      }
    }

    def loop(n: Int): Option[Long] =
      if (n == vec.length - 1) None
      else check(n, n+1, vec(n)) match {
        case None => loop(n + 1)
        case res => res
      }

    loop(0)

  }

  def main(args: Array[String]): Unit = {
    require(args.length == 1)

    val codes = getLines(args(0),
      _.map(n => n.toLong).toList
    )
    // Task 1

    val Some(res) = findNotSum(codes)
    println("First code that is not a sum of previous codes: " + res)

    // Task 2
    println("Sum of smallest and largest summands: " +
      findContiguousSum(res, codes)
    )

  }

}
