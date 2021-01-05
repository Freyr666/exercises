package aoc2020.day24

import aoc2020.utils._

object Solution {

  final case class HexGrid(private val tiles: Map[(Int, Int), HexGrid.Color]) {
    import HexGrid._

    def countBlack: Int =
      tiles.count(_._2 == Black)

    def step: HexGrid = {
      val newTiles: Map[(Int, Int), Color] =
        tiles.flatMap { case (coord, c) =>
          val adj = coord.adj
            .filter(crd => !tiles.contains(crd))
            .map(_ -> HexGrid.White)
          (coord, c) :: adj
        }
      val mapped = newTiles.map { case (coords, c) =>
        val bt = coords
          .adj
          .map(newTiles.getOrElse(_, White))
          .count(_ == Black)
        c match {
          case White if (bt == 2) =>
            (coords, Black)
          case Black if (bt == 0 || bt > 2) =>
            (coords, White)
          case _ => (coords, c)
        }
      }
      HexGrid(mapped)
    }
  }

  object HexGrid {

    def apply(it: Iterator[String]): HexGrid = {
      val m = it.map(stringToDirections)
        .map(dirListToCoord)
        .foldLeft(Map.empty[(Int,Int), Color]) { case (map, coord) =>
          map.get(coord) match {
            case None => map.updated(coord, Black)
            case Some(c) => map.updated(coord, c.opposite)
          }
        }
      HexGrid(m)
    }

    sealed trait Color {
      def opposite: Color =
        this match {
          case Black => White
          case White => Black
        }
    }
    case object Black extends Color
    case object White extends Color

    sealed abstract class Direction(val diff: (Int, Int))
    case object E extends Direction(1, 0)
    case object SE extends Direction(1, -1)
    case object SW extends Direction(0, -1)
    case object W extends Direction(-1, 0)
    case object NW extends Direction(-1, 1)
    case object NE extends Direction(0, 1)

    def stringToDirections(s: String): List[Direction] = {
      sealed trait DecTree { val map: Map[Char, Either[Direction, DecTree]] }
      final case class DT(map: Map[Char, Either[Direction, DecTree]]) extends DecTree
      val decTree = DT(Map(
        'e' -> Left(E),
        'w' -> Left(W),
        's' -> Right(DT(Map(
          'e' -> Left(SE),
          'w' -> Left(SW),
        ))),
        'n' -> Right(DT(Map(
          'e' -> Left(NE),
          'w' -> Left(NW),
        ))),
      ))
      def loop(off: Int, dt: DecTree, acc: List[Direction]): List[Direction] =
        if (off == s.length()) acc.reverse
        else dt.map(s(off)) match {
          case Left(x) => loop(off+1, decTree, x::acc)
          case Right(t) => loop(off+1, t, acc)
        }
      loop(0, decTree, Nil)
    }

    private def dirListToCoord(l: List[Direction]): (Int, Int) = {
      def loop(coord: (Int, Int), l: List[Direction]): (Int, Int) =
        l match {
          case Nil => coord
          case d::tl => {
            val (dx, dy) = d.diff
            loop((coord._1 + dx, coord._2 + dy), tl)
          }
        }
      loop((0, 0), l)
    }

    implicit class Coord(p: (Int, Int)) {
      def +(dir: Direction): (Int, Int) =
        (p._1 + dir.diff._1, p._2 + dir.diff._2)

      def adj: List[(Int, Int)] = List(
        p + E,
        p + W,
        p + NE,
        p + NW,
        p + SE,
        p + SW
      )
    }

  }

  def gridAfterNSteps(grid: HexGrid, n: Int): HexGrid =
    if (n == 0) grid
    else gridAfterNSteps(grid.step, n-1)

  def main(args: Array[String]) = {
    require(args.length == 1)

    val grid = getLines(args(0), HexGrid.apply)
    // Task 1
    println("Black tiles num after flipping: " + grid.countBlack)

    // Task 2
    println("Black tiles num after 100 days: " + gridAfterNSteps(grid, 100).countBlack)
  }

}
