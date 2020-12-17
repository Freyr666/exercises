package aoc2020.day17

import aoc2020.utils._
import java.{util => ju}
import cats.syntax.representable

object Solution {

  trait Engine {
    def step: Engine
    def countActive: Int
  }

  final case class Cube(private val repr: Vector[Vector[Vector[Boolean]]])
      extends Engine {
    require(repr.length != 0)
    require(repr(0).length == repr.length)
    require(repr(0)(0).length == repr.length)

    def step: Cube = {
      val inc = incSize
      val newRepr = inc.zipWithIndex.map { case (segment, z) =>
        segment.zipWithIndex.map { case (row, y) =>
          row.zipWithIndex.map { case (v, x) =>
            val active = (for {
              i <- x-1 to x+1
              j <- y-1 to y+1
              k <- z-1 to z+1
              if (i != x || j != y || k != z)
                } yield get(inc, i, j, k))
              .count(identity)
              v match {
                case true if active == 2 || active == 3 =>
                  true
                case false if active == 3 =>
                  true
                case _ =>
                  false
              }
          }
        }
      }
      Cube(newRepr)
    }

    def countActive: Int = repr.foldLeft(0) { (acc, segment) =>
      segment.foldLeft(acc) { (acc0, row) => acc0 + row.count(identity) }
    }

    private def incSize: Vector[Vector[Vector[Boolean]]] = {
      val size = repr.length + 2
      val margin = Vector.fill(size) {
        Vector.fill(size)(false)
      }
      val middle = repr.map { segment =>
        val margin = Vector.fill(size)(false)
        val middle = segment.map(false +: _ :+ false)
        margin +: middle :+ margin
      }
      margin +: middle :+ margin
    }
    
    private def get(
      repr: Vector[Vector[Vector[Boolean]]],
      x: Int,
      y: Int,
      z: Int
    ): Boolean = {
      if (x < 0 || y < 0 || z < 0) false
      else if (x >= repr.length || y >= repr.length || z >= repr.length) false
      else repr(z)(y)(x)
    }
  }

  object Cube {
    def apply(it: Iterator[String]): Cube = {
      val middle = it
        .map(s => s.toVector.map(_ == '#'))
        .toVector
      val empty = Vector.fill(middle.length) {
        Vector.fill(middle.length)(false)
      }
      val sizePre = middle.length / 2
      val sizePost = middle.length - sizePre - 1
      val repr = Vector.fill(sizePre)(empty) ++ (middle +: Vector.fill(sizePost)(empty))
      Cube(repr = repr)
    }
  }


  final case class HyperCube(private val repr: Vector[Vector[Vector[Vector[Boolean]]]])
      extends Engine {
    require(repr.length != 0)
    require(repr(0).length == repr.length)
    require(repr(0)(0).length == repr.length)
    require(repr(0)(0)(0).length == repr.length)

    def step: HyperCube = {
      val inc = incSize
      val newRepr = inc.zipWithIndex.map { case (cube, w) =>
        cube.zipWithIndex.map { case (segment, z) =>
          segment.zipWithIndex.map { case (row, y) =>
            row.zipWithIndex.map { case (v, x) =>
              val active = (for {
                i <- x-1 to x+1
                j <- y-1 to y+1
                k <- z-1 to z+1
                l <- w-1 to w+1
                if (i != x || j != y || k != z || w != l)
                  } yield get(inc, i, j, k, l))
                .count(identity)
              v match {
                case true if active == 2 || active == 3 =>
                  true
                case false if active == 3 =>
                  true
                case _ =>
                  false
              }
            }
          }
        }
      }
      HyperCube(newRepr)
    }

    def countActive: Int = repr.foldLeft(0) { (acc, cube) =>
      cube.foldLeft(acc) { (acc0, segment) =>
        segment.foldLeft(acc0) { (acc1, row) => acc1 + row.count(identity) }
      }
    }

    private def incSize: Vector[Vector[Vector[Vector[Boolean]]]] = {
      val size = repr.length + 2
      val marginRow = Vector.fill(size)(false)
      val marginSeg = Vector.fill(size)(marginRow)
      val marginCube = Vector.fill(size)(marginSeg)
      val middle = repr.map { cube =>
        val middle = cube.map { segment =>
          val middle = segment.map(false +: _ :+ false)
          marginRow +: middle :+ marginRow
        }
        marginSeg +: middle :+ marginSeg
      }
      marginCube +: middle :+ marginCube
    }
    
    private def get(
      repr: Vector[Vector[Vector[Vector[Boolean]]]],
      x: Int,
      y: Int,
      z: Int,
      w: Int,
    ): Boolean = {
      if (x < 0 || y < 0 || z < 0 || w < 0) false
      else if (x >= repr.length || y >= repr.length || z >= repr.length || w >= repr.length) false
      else repr(w)(z)(y)(x)
    }
  }

  object HyperCube {
    def apply(it: Iterator[String]): HyperCube = {
      val middle = it
        .map(s => s.toVector.map(_ == '#'))
        .toVector
      val emptyRow = Vector.fill(middle.length)(false)
      val emptySeg = Vector.fill(middle.length)(emptyRow)
      val emptyCube = Vector.fill(middle.length)(emptySeg)

      val sizePre = middle.length / 2
      val sizePost = middle.length - sizePre - 1
      val repr3d =
        Vector.fill(sizePre)(emptySeg) ++ (middle +: Vector.fill(sizePost)(emptySeg))
      val repr =
        Vector.fill(sizePre)(emptyCube) ++ (repr3d +: Vector.fill(sizePost)(emptyCube))
      HyperCube(repr = repr)
    }
  }

  def stateAfter6Iters(engine: Engine): Int = {
    def loop(engine: Engine, n: Int = 6): Int =
      if (n == 0) engine.countActive
      else loop(engine.step, n-1)
    loop(engine)
  }

  def main(args: Array[String]) = {
    require(args.length == 1)

    val list = getLines(args(0), _.toList)
    val cube = Cube(list.iterator)
    val hyperCube = HyperCube(list.iterator)

    // Task 1
    println("Cube after 6 iterations " + stateAfter6Iters(cube))

    // Task 2
    println("HyperCube after 6 iterations " + stateAfter6Iters(hyperCube))
  }

}
