package aoc2020.day20

import aoc2020.utils._

object Solution {

  sealed trait Side
  final case object Left extends Side
  final case object Right extends Side
  final case object Top extends Side
  final case object Bottom extends Side

  /*
   *  Represents tile borders, also
   *  contains a list of all applied transformations
   *  (rotations and flips) so that we could apply them
   *  later to the raw tile data
   */
  final case class MapTile(
    val id: Int,
    val top: String,
    val right: String,
    val bottom: String,
    val left: String,
    val trans: List[MapTile.Trans]
  ) {

    def transforms: List[MapTile] = {
      def rotations(tile: MapTile): List[MapTile] = {
        val topRev = tile.top.reverse
        val rightRev = tile.right.reverse
        val bottomRev = tile.bottom.reverse
        val leftRev = tile.left.reverse
        List(
          tile,
          MapTile(tile.id, leftRev, tile.top, rightRev, tile.bottom, MapTile.Rot90::tile.trans), // 90deg
          MapTile(tile.id, bottomRev, leftRev, topRev, rightRev, MapTile.Rot180::tile.trans), // 180deg
          MapTile(tile.id, tile.right, bottomRev, tile.left, topRev, MapTile.Rot270::tile.trans), // 270deg
        )
      }
      List(
        this,
        MapTile(id, top.reverse, left, bottom.reverse, right, MapTile.FlipH::trans), // Horiz
      ).flatMap(rotations)
    }

    def matches(other: MapTile): List[Side] =
      List(
        if (top == other.bottom) List(Top) else Nil,
        if (bottom == other.top) List(Bottom) else Nil,
        if (left == other.right) List(Left) else Nil,
        if (right == other.left) List(Right) else Nil,
      ).flatten
  }

  object MapTile {
    sealed trait Trans
    case object FlipH extends Trans
    case object FlipV extends Trans
    case object Rot90 extends Trans
    case object Rot180 extends Trans
    case object Rot270 extends Trans

    def transform(raw: Vector[Vector[Char]], trans: List[Trans]): Vector[Vector[Char]] =
      trans match {
        case Nil => raw
        case Rot90::rest =>
          transform(raw.transpose, FlipH::rest)
        case Rot180::rest =>
          transform(raw, FlipV::FlipH::rest)
        case Rot270::rest =>
          transform(raw.transpose, FlipV::rest)
        case FlipH::rest =>
          transform(raw.map(_.reverse), rest)
        case FlipV::rest =>
          transform(raw.reverse, rest)
      }

    def apply(id: Int, cont: Vector[String]): MapTile = {
      val top = cont.head
      val bottom = cont.last
      val left = cont.map(_(0)).mkString
      val right = cont.map(_.last).mkString
      MapTile(id, top, right, bottom, left, Nil)
    }
  }

  /*
   * Result of gluing the map pieces together,
   * the whole map.
   */
  final case class MegaTile(data: Vector[Vector[Char]]) {
    override def toString(): String =
      data
        .map(_.mkString)
        .mkString("\n")

    def convolution(pat: Vector[Vector[Char]]): Int = {
      require(pat.length > 0)
      require(pat(0).length > 0)
      require(pat.forall(_.length == pat(0).length))

      val rough = data.indices.foldLeft(Set.empty[(Int,Int)]) { (acc, y) =>
        data(y).indices.foldLeft(acc) { (acc1, x) =>
          if (data(y)(x) == '#') acc1 + (x -> y) else acc1
        }
      }

      def loop(x: Int, y: Int, pat: Vector[Vector[Char]], acc: Set[(Int, Int)]): Set[(Int, Int)] =
        if (y + pat.length > data.length) acc
        else if (x + pat(0).length > data(0).length) loop(0, y+1, pat, acc)
        else {
          val found = pat.indices.forall { yOff =>
            pat(0).indices.forall { xOff =>
              (pat(yOff)(xOff) == '#' && data(y+yOff)(x+xOff) == '#') || pat(yOff)(xOff) == ' '
            }
          }
          val acc1 =
            if (!found) acc
            else pat.indices.foldLeft(acc) { (acc2, yOff) =>
              pat(0).indices.foldLeft(acc2) { (acc3, xOff) =>
                if (pat(yOff)(xOff) == '#') acc3 - ((x+xOff) -> (y+yOff))
                else acc3
              }
            }
          loop(x+1, y, pat, acc1)
        }

      val res = List(
        pat,
        MapTile.transform(pat, MapTile.FlipH::Nil),
        MapTile.transform(pat, MapTile.FlipV::Nil),
        MapTile.transform(pat, MapTile.FlipV::MapTile.FlipH::Nil),
        MapTile.transform(pat, MapTile.Rot90::Nil),
        MapTile.transform(pat, MapTile.Rot90::MapTile.FlipH::Nil),
        MapTile.transform(pat, MapTile.Rot90::MapTile.FlipV::Nil),
        MapTile.transform(pat, MapTile.Rot90::MapTile.FlipV::MapTile.FlipH::Nil),
      ).foldLeft(rough){ (acc, p) =>
        loop(0, 0, p, acc)
      }
      res.size
    }

  }

  /*
   * Grid of separate map tiles, also containing the unused tile.
   * Each call of addMatches constructs a new Grid by fitting one
   * of the unused pieces in its place.
   */
  final case class Grid(grid: Map[(Int, Int), MapTile], unused: List[MapTile]) {

    lazy val xRange: Range = {
      val (xs, _) = grid.keys.unzip
      xs.min to xs.max
    }

    lazy val yRange: Range = {
      val (_, ys) = grid.keys.unzip
      ys.min to ys.max
    }

    def getCornerIds: List[Int] = {
      val (xmin, xmax, ymin, ymax) =
        (xRange.start, xRange.end, yRange.start, yRange.end)
      List(
        grid.get((xmin, ymin)).map(_.id),
        grid.get((xmax, ymin)).map(_.id),
        grid.get((xmin, ymax)).map(_.id),
        grid.get((xmax, ymax)).map(_.id)
      ).flatten
    }

    def addMatches: Option[Grid] = {
      val gridTiles = grid.toList

      def normalize(g: Map[(Int, Int), MapTile]): Map[(Int, Int), MapTile] = {
        val (xmin, ymin) =
          (xRange.start, yRange.start)
        val xdif = if (xmin < 0) -xmin else 0
        val ydif = if (ymin < 0) -ymin else 0
        g.map { case ((xpos, ypos), tile) => ((xpos+xdif, ypos+ydif), tile) }
      }

      def checkNewTile(t: ((Int, Int), MapTile)): Boolean = {
        val ((xpos, ypos), tile) = t
        List(
          grid.get((xpos, ypos+1)).map(_.top == tile.bottom),
          grid.get((xpos, ypos-1)).map(_.bottom == tile.top),
          grid.get((xpos+1, ypos)).map(_.left == tile.right),
          grid.get((xpos-1, ypos)).map(_.right == tile.left),
        ).flatten.forall(identity)
      }

      def matching(tile: MapTile): List[((Int, Int), MapTile)] = {
        gridTiles.flatMap { case ((xpos, ypos), t) =>
          t.matches(tile).map(_ match {
            case Top => ((xpos, ypos - 1), tile)
            case Left => ((xpos - 1, ypos), tile)
            case Bottom => ((xpos, ypos + 1), tile)
            case Right => ((xpos + 1, ypos), tile)
          })
        }
      }

      def findFirst(tiles: List[MapTile]): Option[((Int, Int), MapTile)] =
        tiles match {
          case Nil => None
          case h::tl =>
            matching(h).filter(checkNewTile) match {
              case Nil => findFirst(tl)
              case res::_ => Some(res)
            }
        }

      findFirst(unused.flatMap(_.transforms))
        .map { case (pos, newTile) =>
          Grid(normalize(grid.updated(pos, newTile)), unused.filter(_.id != newTile.id))
        }
    }

    def megaTile(raw: List[(Int, Vector[String])]): MegaTile = {
      val rawMap = raw.toMap.mapValues(_.map(s => s.toVector))

      val cont = yRange.map { y =>
        xRange.map { x =>
          val t = grid.get((x, y)).get
          val (id, trans) = (t.id, t.trans)
          val data = rawMap(id).drop(1).dropRight(1).map(v =>
            v.drop(1).dropRight(1)
          )
          MapTile.transform(data, trans.reverse)
        }.reduce((l, r) => l.zip(r).map { case (a, b) => a ++ b })
      }.reduce(_ ++ _)

      MegaTile(cont)
    }

  }

  def parseTiles(it: Iterator[String]): List[(Int, Vector[String])] = {
    def title(acc: List[(Int, Vector[String])]): List[(Int, Vector[String])] =
      it.nextOption match {
        case None => acc
        case Some(s) =>
          val Array(_, n) = s.split(' ')
          data(n.stripSuffix(":").toInt, acc)
      }
    def data(id: Int, acc: List[(Int, Vector[String])], cont: Vector[String] = Vector.empty): List[(Int, Vector[String])] =
      it.nextOption() match {
        case None => (id, cont)::acc
        case Some("") => title((id, cont)::acc)
        case Some(s) => data(id, acc, cont.appended(s.trim()))
      }
    title(Nil)
  }

  def fitAllTiles(l: List[MapTile]): Grid = {
    def loop(s: Grid): Grid =
      if (s.unused.length == 0) s
      else s.addMatches match {
        case None => ???
        case Some(next) => loop(next)
      }

    loop(Grid(Map((0,0) -> l.head), l.tail))
  }

  val monsterPattern = Vector(
    "                  # ",
    "#    ##    ##    ###",
    " #  #  #  #  #  #   ",
  )

  def main(args: Array[String]) = {
    require(args.length == 1)

    val raw = getLines(args(0), parseTiles)
    val tiles = raw.map(p => MapTile(p._1, p._2))
    // Task 1
    val grid = fitAllTiles(tiles)
    println("Corner tiles id product " +
      grid.getCornerIds.foldLeft(1L)(_ * _)
    )

    // Task 2
    val pat = monsterPattern.map(_.toVector)
    val map = grid.megaTile(raw)
    println("Count non monster #'s " +
      map.convolution(pat)
    )
  }

}
