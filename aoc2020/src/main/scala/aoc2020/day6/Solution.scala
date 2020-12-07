package aoc2020.day6

import cats.effect._
import scala.io.Source
import aoc2020.utils._

object Solution extends IOApp {

  def countAnswers(forms: List[String], pred: (Int, Int) => Boolean): IO[Int] = IO {
    val slots: Array[Int] = Array.fill('z' - 'a' + 1)(0)
    val length = forms.length
    forms.foreach { s =>
      s.foreach(ch => slots('z' - ch) += 1)
    }
    slots.count(v => pred(length, v))
  }

  def countAllAnswers(allForms: List[List[String]], pred: (Int, Int) => Boolean): IO[Int] =
    allForms.foldLeft(IO.pure(0)) { (acc, forms) =>
      acc.flatMap(sum =>
        countAnswers(forms, pred).flatMap(res =>
          IO.pure(sum + res)))
    }

  def run(args: List[String]): IO[ExitCode] = {
    require(args.nonEmpty)

    val src = source(args.head)
    for {
      data <- src.use(src => IO {
        splitLines(src.getLines())
      })
      answersNum <- countAllAnswers(data, { (_,num) =>
        num > 0
      })
      _ <- IO(println("All answers: " + answersNum))
      rightAnswersNum <- countAllAnswers(data, { (length, x) =>
        x == length
      })
      _ <- IO(println("All right answers: " + rightAnswersNum))
    } yield ExitCode.Success
  }

}
