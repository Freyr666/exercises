package aoc2020.day7

import aoc2020.utils._
import scala.collection.mutable.HashMap

object Solution {

  val test1 = Seq(
    "light red bags contain 1 bright white bag, 2 muted yellow bags.",
    "dark orange bags contain 3 bright white bags, 4 muted yellow bags.",
    "bright white bags contain 1 shiny gold bag.",
    "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.",
    "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.",
    "dark olive bags contain 3 faded blue bags, 4 dotted black bags.",
    "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.",
    "faded blue bags contain no other bags.",
    "dotted black bags contain no other bags."
  )

  val test2 = Seq(
    "shiny gold bags contain 2 dark red bags.",
    "dark red bags contain 2 dark orange bags.",
    "dark orange bags contain 2 dark yellow bags.",
    "dark yellow bags contain 2 dark green bags.",
    "dark green bags contain 2 dark blue bags.",
    "dark blue bags contain 2 dark violet bags.",
    "dark violet bags contain no other bags."
  )

  def parseLine(s: String): (String, List[(Int, String)]) = {
    val Array(outer, inner) = s.split(" contain ")
    val bag = outer.stripSuffix(" bags")
    val contents =
      if (inner == "no other bags.") Nil
      else inner
        .stripSuffix(".")
        .split(", ")
        .map { bag =>
          val withNum = bag
            .stripSuffix(" bags")
            .stripSuffix(" bag")
          val (n, color) = withNum
            .splitAt(withNum.indexOf(' '))
          (n.toInt, color.trim())
        }
        .toList
    (bag, contents)
  }

  def makeDigraph(it: Iterator[String]):
      (Digraph[String], HashMap[String, List[(Int, String)]]) = {
    val graph = new Digraph[String]
    val table = new HashMap[String, List[(Int, String)]]
    it.map(parseLine)
      .foreach { case (outer, inners) =>
        table(outer) = inners
        inners.foreach(in =>
          graph.addEdge(in._2, outer)
        )
      }
    (graph, table)
  }

  def countTotalPackages(
    root: String,
    table: HashMap[String, List[(Int, String)]]
  ): Long = {
    1L + table
      .getOrElse(root, Nil)
      .foldLeft(0L) { case (acc, (mult, color)) =>
        acc + mult * countTotalPackages(color, table)
      }
  }

  def main(args: Array[String]): Unit = {
    require(args.length == 1)

    val (dg, table) = getLines(args(0), makeDigraph)

    println("Count outer bags: " +
      (dg.countReachable("shiny gold") - 1)
    )

    println("Total packages: " +
      (countTotalPackages("shiny gold", table) - 1)
    )

  }

}
