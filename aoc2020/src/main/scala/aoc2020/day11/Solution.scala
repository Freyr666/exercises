package aoc2020.day11

import aoc2020.utils._

object Solution {

  sealed trait Seat {
    override def toString(): String =
      this match {
        case Empty => "L"
        case Occupied => "#"
        case _ => "."
      }
  }
  case object Floor extends Seat
  case object Occupied extends Seat
  case object Empty extends Seat

  object Seat {
    def apply(c: Char): Seat =
      c match {
        case 'L' => Empty
        case '#' => Occupied
        case _ => Floor
      }
  }

  final class Grid(private val seats: Array[Array[Seat]]) {
    assert(seats.length > 0)
    val length = seats.length
    val width = seats(0).length
    assert(seats.forall(_.length == width))

    override def equals(that: Any): Boolean =
      that match {
        case that: Grid =>
          length == that.length &&
          width == that.width &&
          seats.zip(that.seats).forall {
            case (r, thatr) =>
              r.zip(thatr).forall {
                case (seat, other) => seat == other
              }
          }
        case _ => false
      }

    def iterator: Iterator[Seat] =
      seats.iterator.flatMap(_.iterator)

    def update(f: (Int, Int, Seat) => Seat): Grid = {
      val a = for {
        r <- 0 until length
      } yield for {
        c <- 0 until width
      } yield f(r, c, seats(r)(c))
      new Grid(a.map(_.toArray).toArray)
    }

    def seat(row: Int, col: Int): Option[Seat] =
      if (row < 0 || row >= length) None
      else if (col < 0 || col >= width) None
      else Some(seats(row)(col))

    def seat(coords: (Int, Int)): Option[Seat] =
      seat(coords._1, coords._2)

    def adj(row: Int, col: Int): List[Seat] = {
      val pos = for {
        r <- (row - 1) to (row + 1)
        c <- (col - 1) to (col + 1)
        if (! (row == r && col == c))
      } yield (r, c)
      pos.flatMap(p => seat(p._1, p._2)).toList
    }

    def adjNotFloor(row: Int, col: Int): List[(Int, Int)] = {
      def moveWhileFroor(row: Int, col: Int, dir: (Int, Int)): Option[(Int, Int)] =
        seat(row, col).flatMap {
          case Floor =>
            moveWhileFroor(row + dir._1, col + dir._2, dir)
          case _ => Some(row, col)
        }
      val dir = List(
        1 -> -1,
        1 -> 0,
        1 -> 1,
        0 -> 1,
        -1 -> 1,
        -1 -> 0,
        -1 -> -1,
        0 -> -1
      )
      dir.flatMap(d => moveWhileFroor(row + d._1, col + d._2, d))
    }
  }

  object Grid {
    def apply(it: Iterator[String]): Grid = {
      val a = it.map(_.toArray.map(Seat.apply)).toArray
      new Grid(a)
    }
  }

  // Part 1
  def countOccupied(seats: Grid): Int = {
    def countSeats(grid: Grid): Int =
      grid.iterator.count(_ == Occupied)

    def loop(grid: Grid): Int = {
      val newGrid = grid.update { case (r, c, seat) =>
        seat match {
          case Floor => Floor
          case Occupied =>
            if (grid.adj(r, c).count(_ == Occupied) >= 4) Empty
            else Occupied
          case Empty =>
            if (grid.adj(r, c).forall(_ != Occupied)) Occupied
            else Empty
        }
      }
      if (newGrid == grid) countSeats(newGrid)
      else loop(newGrid)
    }

    loop(seats)
  }

  // Part 2
  def countOccupiedDistant(seats: Grid): Int = {
    def countSeats(grid: Grid): Int =
      grid.iterator.count(_ == Occupied)

    def precalcAdj(grid: Grid): Map[(Int, Int), List[(Int, Int)]] = {
      val rel = for {
        i <- 0 until grid.length
        j <- 0 until grid.width
      } yield ((i, j), grid.adjNotFloor(i, j))
      rel.toMap
    }

    def loop(grid: Grid, adj: Map[(Int, Int), List[(Int, Int)]]): Int = {
      val newGrid = grid.update { case (r, c, seat) =>
        seat match {
          case Floor => Floor
          case Occupied =>
            if (adj((r, c))
              .flatMap(grid.seat)
              .count(_ == Occupied) >= 5) Empty
            else Occupied
          case Empty =>
            if (adj((r, c))
              .flatMap(grid.seat)
              .forall(_ != Occupied)) Occupied
            else Empty
        }
      }
      if (newGrid == grid) countSeats(newGrid)
      else loop(newGrid, adj)
    }

    loop(seats, precalcAdj(seats))
  }

  def main(args: Array[String]): Unit = {
    require(args.length == 1)

    val seats = getLines(args(0), Grid.apply)
    // Task 1
    println("Occupied seats: " + countOccupied(seats))

    // Task 2
    println("Occupied seats upd rules: " + countOccupiedDistant(seats))
  }

}
