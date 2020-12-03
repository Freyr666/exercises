package aoc2020.week1

import cats.effect._
import scala.collection.mutable.Stack
import scala.io.Source
import scala.concurrent.ExecutionContext

private class Table {
  private val array: Array[Stack[Int]] =
    Array.tabulate(10)(_ => Stack.empty)

  def add(x: Int): Unit =
    array(x % 10).push(x)

  def exists(x: Int): Boolean =
    array(x % 10).exists(_ == x)
}

object Solution extends IOApp {

  def source(f: String): Resource[IO, Source] =
    Resource.fromAutoCloseable(IO(Source.fromFile(f)))

  def sumOfTwo(a: Array[Int]): Option[(Int, Int)] = {
    val table = new Table

    a.find { x =>
      if (table.exists(2020 - x)) true
      else {
        table.add(x)
        false
      }
    }.map(x => (x, 2020 - x))

  }

  def sumOfThree(a: Array[Int]): Option[(Int, Int, Int)] = {
    require(a.length > 2)
    val s = a.sorted

    def recur(i: Int, j: Int, k: Int): Option[(Int, Int, Int)] = {
      if (j >= k) None
      else {
        val sum = s(i) + s(j) + s(k)
        if (sum == 2020)
          Some(i, j, k)
        else if (sum > 2020)
          recur(i, j, k - 1)
        else if (sum < 2020 && j - i > 1)
          recur(i + 1, j, k)
        else
          recur(i, j + 1, k)
      }
    }
    recur(0, 1, a.length - 1).map { case (i, j, k) =>
      (s(i), s(j), s(k))
    }
  }

  def readArray(src: Source): IO[Array[Int]] =
    IO(src.getLines()
      .flatMap(_.toIntOption)
      .toArray)

  def run(args: List[String]): IO[ExitCode] = {
    require(args.nonEmpty)

    for {
      data <- source(args.head).use(readArray)
      two = sumOfTwo(data).map(p => p._1 * p._2)
      _ <- IO(println("Product of two: " + two))
      three = sumOfThree(data).map(p => p._1 * p._2 * p._3)
      _ <- IO(println("Product of three: " + three))
    } yield ExitCode.Success
  }

}
