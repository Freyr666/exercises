package aoc2020.week3

import cats.effect._
import scala.io.Source

object Solution extends IOApp {

  def source(f: String): Resource[IO, Source] =
    Resource.fromAutoCloseable(IO(Source.fromFile(f)))

  sealed trait Ground
  final case object Tree extends Ground
  final case object Plane extends Ground

  final class Space(field: Array[Array[Ground]]) {
    require(field.length > 0)
    require(field(0).length > 0)
    val width = field(0).length
    val distance: Int = field.length
    def ground(xpos: Int, ypos: Int): Ground =
      field(ypos)(xpos % width)
  }

  object Space {
    def apply(src: Source): IO[Space] = {
      val arr = IO(src
        .getLines()
        .map(string =>
          string
            .iterator
            .map { case '#' => Tree case _ => Plane }
            .toArray[Ground] )
        .toArray)
      arr.map(new Space(_))
    }
  }

  def countTrees(space: Space,
    dcol: Int, drow: Int,
    col: Int = 0, row: Int = 0,
    num: Int = 0): Int =
    if (row >= space.distance) num
    else space.ground(col, row) match {
      case Plane =>
        countTrees(space, dcol, drow, col + dcol, row + drow, num)
      case Tree =>
        countTrees(space, dcol, drow, col + dcol, row + drow, num + 1)
    }

  def multAllSlopes(space: Space): Long = {
    val slopes = Seq(
      1 -> 1,
      3 -> 1,
      5 -> 1,
      7 -> 1,
      1 -> 2
    )
    slopes.foldLeft(1L) { (acc, delta) =>
      acc * countTrees(space, delta._1, delta._2)
    }
  }

  def run(args: List[String]): IO[ExitCode] = {
    require(args.nonEmpty)

    val src = source(args.head)
    for {
      forest <- src.use(Space(_))
      treesNum = countTrees(forest, 3, 1)
      _ <- IO(println("Number of trees: " + treesNum))
      allSlopes = multAllSlopes(forest)
      _ <- IO(println("Mult all slopes: " + allSlopes))
    } yield ExitCode.Success

  }

}
