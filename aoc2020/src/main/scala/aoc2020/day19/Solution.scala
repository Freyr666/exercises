package aoc2020.day19

import aoc2020.utils._
import scala.collection.immutable.HashMap
import scala.collection.immutable.Set

object Solution {

  class Rexp(table: HashMap[Int, Rexp.Symbol]) {

    def matches(s: String): Boolean = {
 
      def update(state: (Int, List[Int])): List[(Int, List[Int])] =
        if (state._1 == s.length()) Nil
        else state._2 match {
          case Nil => Nil
          case ind::tl => table(ind) match {
            case Rexp.Term(c) if c == s(state._1) =>
              List((state._1+1, tl))
            case Rexp.Term(_) =>
              Nil
            case Rexp.Rules(l) =>
              l.map(x => (state._1, x ++ tl))
          }
        }

      def loop(state: Set[(Int, List[Int])]): Boolean = {
        if (state.isEmpty) false
        else if (state.contains((s.length(), Nil))) true
        else loop(state.flatMap(update))
      }

      loop(Set((0, List(0))))
    }

  }

  object Rexp {
    private sealed trait Symbol
    private final case class Term(c: Char) extends Symbol
    private final case class Rules(l: List[List[Int]]) extends Symbol

    def apply(s: List[String]): Rexp = {
      def toRelation(s: String): (Int, Symbol) = {
        val Array(ind, rules) = s.split(':').map(_.trim())
        val vars =
          if (rules.contains('"')) Term(rules(1))
          else if (rules.contains('|')) Rules(rules
            .split('|')
            .map(l => l.trim().split(' ').map(_.toInt).toList)
            .toList
          )
          else Rules(List(rules.split(' ').map(_.toInt).toList))
        (ind.toInt, vars)
      }
      val table =
        s
          .map(toRelation)
          .foldLeft(HashMap.empty[Int, Symbol]) { _ + _ }
      new Rexp(table)
    }
  }

  val test1 = List(
    "42: 9 14 | 10 1",
    "9: 14 27 | 1 26",
    "10: 23 14 | 28 1",
    "1: \"a\"",
    "11: 42 31",
    "5: 1 14 | 15 1",
    "19: 14 1 | 14 14",
    "12: 24 14 | 19 1",
    "16: 15 1 | 14 14",
    "31: 14 17 | 1 13",
    "6: 14 14 | 1 14",
    "2: 1 24 | 14 4",
    "0: 8 11",
    "13: 14 3 | 1 12",
    "15: 1 | 14",
    "17: 14 2 | 1 7",
    "23: 25 1 | 22 14",
    "28: 16 1",
    "4: 1 1",
    "20: 14 14 | 1 15",
    "3: 5 14 | 16 1",
    "27: 1 6 | 14 18",
    "14: \"b\"",
    "21: 14 1 | 1 14",
    "25: 1 1 | 1 14",
    "22: 14 14",
    "8: 42",
    "26: 14 22 | 1 20",
    "18: 15 15",
    "7: 14 5 | 1 21",
    "24: 14 1",
    "",
    "abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa",
    "bbabbbbaabaabba",
    "babbbbaabbbbbabbbbbbaabaaabaaa",
    "aaabbbbbbaaaabaababaabababbabaaabbababababaaa",
    "bbbbbbbaaaabbbbaaabbabaaa",
    "bbbababbbbaaaaaaaabbababaaababaabab",
    "ababaaaaaabaaab",
    "ababaaaaabbbaba",
    "baabbaaaabbaaaababbaababb",
    "abbbbabbbbaaaababbbbbbaaaababb",
    "aaaaabbaabaaaaababaa",
    "aaaabbaaaabbaaa",
    "aaaabbaabbaaaaaaabbbabbbaaabbaabaaa",
    "babaaabbbaaabaababbaabababaaab",
    "aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba",
  )

  def parseData(it: Iterator[String]): (Rexp, List[String]) = {
    def readRexp(acc: List[String]): (Rexp, List[String]) =
      it.nextOption() match {
        case None => ???
        case Some("") => readVals(Rexp(acc), Nil)
        case Some(l) => readRexp(l::acc)
      }
    def readVals(r: Rexp, acc: List[String]): (Rexp, List[String]) =
      it.nextOption() match {
        case None | Some("") => (r, acc.reverse)
        case Some(l) => readVals(r, l::acc)
      }
    readRexp(Nil)
  }

  def parseDataUpdated(it: Iterator[String]): (Rexp, List[String]) = {
    def readRexp(acc: List[String]): (Rexp, List[String]) =
      it.nextOption() match {
        case None => ???
        case Some("") => readVals(Rexp(acc), Nil)
        case Some("8: 42") => readRexp("8: 42 | 42 8"::acc)
        case Some("11: 42 31") => readRexp("11: 42 31 | 42 11 31"::acc)
        case Some(l) => readRexp(l::acc)
      }
    def readVals(r: Rexp, acc: List[String]): (Rexp, List[String]) =
      it.nextOption() match {
        case None | Some("") => (r, acc.reverse)
        case Some(l) => readVals(r, l::acc)
      }
    readRexp(Nil)
  }

  def main(args: Array[String]) = {
    require(args.length == 1)

    val data = getLines(args(0), _.toList)

    val (rexp, lines) = parseData(data.iterator)

    // Task 1
    println("Number of matching lines: " + lines.count(l => rexp.matches(l)))

    // Task 2
    val (rexp1, lines1) = parseDataUpdated(data.iterator)
    println("Number of matching lines after rule update " + lines1.count(l => rexp1.matches(l)))

  }

}
