package aoc2020.day5

import aoc2020.utils
import cats.effect._

object Solution extends IOApp {

  final case class Seat(row: Int, chair: Int, id: Int)

  object Seat {

    def apply(s: String): Seat = {
      def narrowSearch(off: Int, f: Int, b: Int, l: Int, r: Int): (Int, Int) =
        if (off == s.length()) (f, l)
        else s(off) match {
          case 'F' =>
            narrowSearch(off + 1, f, f + (b - f) / 2, l, r)
          case 'B' =>
            narrowSearch(off + 1, f + (b - f) / 2, b, l, r)
          case 'L' =>
            narrowSearch(off + 1, f, b, l, l + (r - l) / 2)
          case 'R' =>
            narrowSearch(off + 1, f, b, l + (r - l) / 2, r)
        }
      val (row, chair) = narrowSearch(0, 0, 128, 0, 8)
      Seat(row, chair, row * 8 + chair)
    }

  }

  final class Deck {
    val deck: Array[(Int, Int, List[Seat])] =
      Array.tabulate(128)(i => (0, i, Nil))
    var setIds: Set[Int] = Set.empty

    def addSeat(seat: Seat): IO[Unit] =
      IO {
        require(seat.chair >= 0 && seat.chair < 8)
        val (num, i, seats) = deck(seat.row)
        if (! seats.exists(_.id == seat.id)) {
          deck(seat.row) = (num + 1, i, seat::seats)
          setIds += seat.id
        }
      }

    def missingSeat: IO[List[Seat]] =
      IO {
        deck
          .filter { case (num, i, _) => i > 0 && i < 127 && num < 8 }
          .toList
          .flatMap { case (_, row, seats) =>
            complement(row, 0, seats.sortBy(_.chair), Nil)
          }
      }

    private def complement(row: Int, chair: Int, seats: List[Seat], acc: List[Seat]): List[Seat] = {
      val id = row * 8 + chair
      seats match {
        case s::rest if s.chair != chair && setIds.exists(_ == id + 1) && setIds.exists(_ == id - 1) =>
          complement(row, chair + 1, seats, Seat(row, chair, row * 8 + chair)::acc)
        case s::rest =>
          complement(row, chair + 1, rest, acc)
        case Nil if chair < 8 && setIds.exists(_ == id + 1) && setIds.exists(_ == id - 1) =>
          Seat(row, chair, row * 8 + chair)::acc
        case Nil =>
          acc
      }
    }
  }



  def run(args: List[String]): IO[ExitCode] = {
    require(args.nonEmpty)

    val data = utils.source(args.head)

    for {
      seats <- data.use(src => IO(src.getLines().map(Seat(_)).toList))
      _ <- IO(println("Max seat: " + seats.maxBy(_.id)))
      deck = new Deck
      _ <- seats.foldLeft(IO.pure(())) { case (eff, el) =>
        eff.flatMap(_ => deck.addSeat(el))
      }
      missing <- deck.missingSeat
      _ <- IO(println("Missing seat: " + missing))
    } yield ExitCode.Success
  }

}
