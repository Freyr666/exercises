package aoc2020.day21

import aoc2020.utils._

object Solution {

  def parseRecipes(it: Iterator[String]): List[(List[String], List[String])] = {
    def parseIngredients(s: String, start: Int = 0, end: Int = 0, acc: List[String] = Nil): (List[String], List[String]) =
      if (end == s.length()) {
        if (end == start) (acc.reverse, Nil)
        else ((s.substring(start, end)::acc).reverse, Nil)
      } else s(end) match {
        case ' ' =>
          parseIngredients(s, end+1, end+1, s.substring(start, end)::acc)
        case '(' =>
          (acc.reverse, parseAllergens(s.substring(end)))
        case _ =>
          parseIngredients(s, start, end+1, acc)
      }
    def parseAllergens(s: String): List[String] =
      s
        .stripPrefix("(contains ")
        .stripSuffix(")")
        .split(", ")
        .toList
    it.map(s => parseIngredients(s)).toList
  }

  def countNonAllergens(rec: List[(List[String], List[String])]): Long = {
    val table = rec.foldLeft(Map.empty[String, Set[String]]) { case (map, (ingreds, allerg)) =>
      allerg.foldLeft(map){ (map, k) =>
        map.get(k) match {
	  case None => map.updated(k, ingreds.toSet)
          case Some(s) => map.updated(k, ingreds.toSet & s)
        }
      }
    }
    val allergens =
      table.values.reduce(_ | _)

    rec.foldLeft(0L) { case (acc, (ingreds, _)) =>
      ingreds.foldLeft(acc) { case (acc1, i) =>
        if (allergens(i)) acc1
        else acc1 + 1
      }
    }
  }

  def allergenList(rec: List[(List[String], List[String])]): String = {
    def unique(uniq: Map[String, Set[String]], ambig: Map[String, Set[String]]): List[(String, String)] =
      if (ambig.isEmpty) uniq.mapValues(_.head).toList
      else {
        val set = uniq.foldLeft(Set.empty[String])((acc, kv) => acc | kv._2)
        val map = ambig.mapValues(_ -- set)
        val (newUniq, newAmbig) = map.partition { case (_, v) =>
          v.size == 1
        }
        unique(uniq ++ newUniq, newAmbig.toMap)
      }

    val table = rec.foldLeft(Map.empty[String, Set[String]]) { case (map, (ingreds, allerg)) =>
      allerg.foldLeft(map){ (map, k) =>
        map.get(k) match {
	  case None => map.updated(k, ingreds.toSet)
          case Some(s) => map.updated(k, ingreds.toSet & s)
        }
      }
    }

    unique(Map.empty, table)
      .view
      .sortBy(_._1) // Sort by allergen type
      .map(_._2)    // Take ingredient
      .mkString(",")// Concat ingredients
  }

  def main(args: Array[String]) = {
    require(args.length == 1)

    val recipes = getLines(args(0), parseRecipes)
    // Task 1
    println("Non allergens number: " + countNonAllergens(recipes))

    // Task 2
    println("Allergen list: " + allergenList(recipes))
  }

}
