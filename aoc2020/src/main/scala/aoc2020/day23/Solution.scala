package aoc2020.day23

import collection.mutable.HashMap
import cats.effect._
import aoc2020.utils._

object Solution {

  final class Ring(
    private val table: HashMap[Int, Ring.Node],
    private val upperBound: Int,
    private var first: Ring.Node
  ) {
    import Ring._

    def shiftLeft: IO[Unit] = IO{
      first = first.tail.get
    }

    def toList: List[Int] = {
      def collect(node: Node, acc: List[Int]): List[Int] =
        if (node.head == first.head) acc.reverse
        else collect(node.tail.get, node.head::acc)
      collect(first.tail.get, List(first.head))
    }

    def removeSubRing(len: Int): IO[SubRing] = IO{
      require(len > 0)

      def lastNode(node: Node, len: Int): Node =
        if (len == 0) node
        else lastNode(node.tail.get, len-1)

      def removeFromTable(node: Option[Node]): Unit =
        node.foreach { n =>
          table.remove(n.head)
          removeFromTable(n.tail)
        }

      val node = first.tail.get
      val last = lastNode(node, len-1)
      first.tail = last.tail
      last.tail = None
      removeFromTable(Some(node))
      node
    }

    def current: IO[Int] = IO(first.head)

    def insertAfter(elem: Int, sub: SubRing): IO[Unit] = IO{

      def findNode(elem: Int): Node =
        table.get(elem) match {
          case Some(node) => node
          case None => findNode(Math.floorMod(elem-1, upperBound))
        }

      def updateTable(node: Option[Node]): Unit =
        node match {
          case None => ()
          case Some(n) => {
            table.update(n.head, n)
            updateTable(n.tail)
          }
        }

      def lastNode(node: Node): Node =
        node.tail match {
          case None => node
          case Some(t) => lastNode(t)
        }

      val node = findNode(elem)
      val nextAfter = node.tail
      val last = lastNode(sub)
      updateTable(Some(sub))
      node.tail = Some(sub)
      last.tail = nextAfter
    }

    def collectAfter(elem: Int, len: Int): IO[List[Int]] = IO{
      def takeN(node: Node, len: Int, acc: List[Int]): List[Int] =
        if (len == 0) acc.reverse
        else takeN(node.tail.get, len-1, node.head::acc)
      table.get(elem) match {
        case None => List()
        case Some(node) => takeN(node.tail.get, len, List())
      }
    }

  }

  object Ring {
    final class Node(val head: Int, var tail: Option[Node])

    type SubRing = Node

    def apply(init: Seq[Int]): IO[Ring] = IO{
      require(! init.isEmpty)

      val table = new HashMap[Int, Node]
      val node = new Node(head = init.head, tail = None)
      table.update(node.head, node)
      val last = init.tail.foldLeft(node) { (n, el) =>
        val newNode = new Node(head = el, tail = None)
        table.update(el, newNode)
        n.tail = Some(newNode)
        newNode
      }
      last.tail = Some(node)
      new Ring(table, init.max+1, node)
    }
  }

  def simpleRotationRing(cups: Vector[Int], cupsToCollect: Int, rotations: Int): IO[List[Int]] = {
    def loop(cups: Ring, step: Int): IO[Unit] =
      if (step == 0) IO(())
      else (for {
        sub <- cups.removeSubRing(3)
        cur <- cups.current
        _   <- cups.insertAfter(cur-1, sub)
        _   <- cups.shiftLeft
      } yield ()).flatMap { _ =>
        loop(cups, step-1)
      }

    for {
      ring <- Ring(cups)
      _    <- loop(ring, rotations)
      res  <- ring.collectAfter(1, cupsToCollect)
    } yield res

  }

  def simpleRotation(cups: Vector[Int], rotations: Int): Vector[Int] = {
    def insertAfter(dest: Int, inUse: Vector[Int], rest: Vector[Int]): Vector[Int] = {
      val (before, after) = inUse.splitAt(inUse.indexOf(dest))
        before ++ rest ++ after
    }
    def loop(cups: Vector[Int], step: Int): Vector[Int] = {
      if (step == 0) cups
      else cups match {
        case cur+:rest => {
          val (rem, inUse) = rest.splitAt(3)
          val dest = inUse.filter(_ < cur).maxOption match {
            case Some(v) => v
            case None => inUse.max
          }
          loop(insertAfter(dest, inUse, rem) :+ cur, step-1)
        }
      }
    }
    loop(cups, rotations)
  }

  def generateCups(init: Vector[Int]): Vector[Int] =
    Vector.tabulate(1000000) { ind =>
      if (ind < init.length) init(ind)
      else ind + 1
    }



  def main(args: Array[String]) = {
    require(args.length == 1)

    val cups = args(0)
      .toVector
      .map(c => (c - '0').toInt)

    // Task I
    val res = simpleRotation(cups, 100)
    val (beforeOne, afterOne) = res.splitAt(res.indexOf(1))
    println("Resulting sequence is " +
      (afterOne.tail ++ beforeOne).mkString
    )

    // Task II
    val allCups = generateCups(cups)
    val List(a, b) = simpleRotationRing(allCups, 2, 10000000).unsafeRunSync()
    println("Product of two cups after 1 is " +
      (a.toLong * b.toLong)
    ) 

  }

}
