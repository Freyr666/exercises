package aoc2020.day2

import cats.effect._
import scala.io.Source

object Solution extends IOApp {

  def source(f: String): Resource[IO, Source] =
    Resource.fromAutoCloseable(IO(Source.fromFile(f)))

  final case class Pass(
    lo: Int,
    hi: Int,
    char: Char,
    word: String
  )

  def parsePass(s: String): Pass = {
    val Array(l, r) = s.split(":", 2)
    val word = r.trim()
    val Array(l1, r1) = l.split(" ", 2)
    val char = r1(0)
    val Array(lo, hi) = l1.split("-", 2).map(_.toInt)
    Pass(lo, hi, char, word)
  }

  def validPass(s: String): Boolean = {
    val Pass(lo, hi, char, word) = parsePass(s)
    val occur = word.count(_ == char)
    occur >= lo && occur <= hi
  }

  def countValid(data: Seq[String], validator: String => Boolean): IO[Int] =
    IO.pure(data.count(validator))

  def validPassEnhanced(s: String): Boolean = {
    val Pass(lo, hi, char, word) = parsePass(s)
    word(lo - 1) == char ^ word(hi - 1) == char
  }

  def run(args: List[String]): IO[ExitCode] = {
    require(args.nonEmpty)

    val data = source(args.head)
    for {
      lines <- data.use(src => IO(src.getLines().toSeq))
      count <- countValid(lines, validPass)
      _ <- IO(println("Number of valid passwords: " + count))
      count2 <- countValid(lines, validPassEnhanced)
      _ <- IO(println("Number of valid passwords part two: " + count2))
    } yield ExitCode.Success

  }

}
