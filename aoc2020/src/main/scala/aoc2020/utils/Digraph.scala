package aoc2020.utils

import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet

class Digraph[T] {

  private val table: HashMap[T, List[T]] = HashMap.empty

  def addEdge(i: T, j: T): Unit = {
    val bucket = table.getOrElse(i, Nil)
    if (! bucket.exists(_ == j))
      table(i) = j::bucket
  }

  def adj(i: T): Iterable[T] =
    table.getOrElse(i, Nil)

  def countReachable(src: T): Int = {
    val marked = HashSet.empty[T]
    def count(src: T): Unit =
      if (marked(src)) ()
      else {
        marked += src
        adj(src).foreach(count)
      }
    count(src)
    marked.size
  }

  def fold[B](z: B)(root: T, f: (B,T) => B): B =
    adj(root).foldLeft(z)((acc, newRoot) =>
      fold(acc)(newRoot, f)
    )

  def reverse: Digraph[T] = {
    val res = new Digraph[T]

    for (key <- table.keys) {
      table(key).foreach(res.addEdge(_, key))
    }

    res
  }

}
