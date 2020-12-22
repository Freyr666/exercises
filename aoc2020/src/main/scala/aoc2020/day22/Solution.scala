package aoc2020.day22

import aoc2020.utils._

object Solution {

  sealed abstract class PlayerID {
    def opposit: PlayerID = this match {
      case P1 => P2
      case P2 => P1
    }
  }
  case object P1 extends PlayerID
  case object P2 extends PlayerID

  type Deck = List[Int]
  type Game = (Deck, Deck)

  implicit class TupleID(t: Game) {
    def deck(id: PlayerID): Deck =
      id match {
        case P1 => t._1
        case P2 => t._2
      }
    def updateSelection(id: PlayerID, f: Deck => Deck) =
      id match {
        case P1 => (f(t._1), t._2)
        case P2 => (t._1, f(t._2))
      }
  }

  def parsePlayersHands(it: Iterator[String]): (List[Int], List[Int]) = {
    def player1(acc: List[Int] = Nil): (List[Int], List[Int]) =
      it.nextOption() match {
        case None => ???
        case Some("") => player2(acc.reverse)
        case Some("Player 1:") => player1(acc)
        case Some(n) => player1(n.toInt::acc)
      }
    def player2(p1: List[Int], acc: List[Int] = Nil): (List[Int], List[Int]) =
      it.nextOption() match {
        case None | Some("") => (p1, acc.reverse)
        case Some("Player 2:") => player2(p1, acc)
        case Some(n) => player2(p1, n.toInt::acc)
      }
    player1()
  }

  def computeScore(deck: Deck): Long =
    deck
      .reverse
      .zipWithIndex
      .foldLeft(0L){ case (acc, (n, ind)) =>
        acc + (n * (ind + 1))
      }

  def winnersScore(p1: Deck, p2: Deck): Long = {
    def play(p1: Deck, p2: Deck): Deck =
      (p1, p2) match {
        case (Nil, w) => w
        case (w, Nil) => w
        case (x::p1tl, y::p2tl) =>
          if (x > y) play(p1tl ++ List(x, y), p2tl)
          else play(p1tl, p2tl ++ List(y, x))
      }
    computeScore(play(p1, p2))
  }

  def recursiveWinnersScore(p1: Deck, p2: Deck): Long = {

    def updateDecks(game: Game, won: PlayerID, lost: PlayerID): Game = {
      val bottom = List(game.deck(won).head, game.deck(lost).head)
      val rest = (game._1.tail, game._2.tail)
      rest.updateSelection(won, _ ++ bottom)
    }

    def roundWinner(h1: Int, h2: Int, tl1: Deck, tl2: Deck): PlayerID =
      if (h1 <= tl1.length && h2 <= tl2.length) play((tl1.take(h1), tl2.take(h2)))._1
      else if (h1 > h2) P1 else P2

    def play(game: Game, history: Set[Game] = Set.empty): (PlayerID, Deck) =
      game match {
        case _ if history.contains(game) => (P1, game.deck(P1))
        case (_, Nil) => (P1, game.deck(P1))
        case (Nil, _) => (P2, game.deck(P2))
        case (x::tl1, y::tl2) => {
          val winnerId = roundWinner(x, y, tl1, tl2)
          val nextGame = updateDecks(game, winnerId, winnerId.opposit)
          play(nextGame, history + game)
        }
      }

    computeScore(play((p1, p2))._2)
  }

  def main(args: Array[String]) = {
    require(args.length == 1)

    val (p1, p2) = getLines(args(0), parsePlayersHands)

    // Task 1
    println("Winner's score after a game: " + winnersScore(p1, p2))

    // Task 2
    println("Winner's score after recursive game(s): " + recursiveWinnersScore(p1, p2))
  }

}
