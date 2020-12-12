package aoc2020.day12

import aoc2020.utils._

object Solution {

  final case class Pos(lon: Int, lat: Int) {
    def dist: Int = Math.abs(lon) + Math.abs(lat)
    def *(mat: (Int, Int, Int, Int)): Pos =
      Pos(lon*mat._1 + lat*mat._2, lon*mat._3 + lat*mat._4)
    def +(that: Pos): Pos =
      Pos(lon+that.lon, lat+that.lat)
    def *(times: Int): Pos =
      Pos(lon*times, lat*times)
    def add(dir: Dir, dist: Int): Pos =
      dir match {
        case E => Pos(lon + dist, lat)
        case N => Pos(lon, lat + dist)
        case W => Pos(lon - dist, lat)
        case S => Pos(lon, lat - dist)
      }
  }

  sealed trait Dir {
    def toDeg: Int = this match {
      case E => 0
      case N => 90
      case W => 180
      case S => -90
    }
    def rotateL(deg: Int): Dir =
      (Math.floorMod(this.toDeg + deg, 360) / 90) match {
        case 0 => E
        case 1 => N
        case 2 => W
        case _ => S
      }
    def rotateR(deg: Int): Dir = rotateL(-deg)
  }
  case object E extends Dir
  case object N extends Dir
  case object W extends Dir
  case object S extends Dir

  sealed trait Command
  final case class Move(dir: Dir, dist: Int) extends Command
  final case class Forward(dist: Int) extends Command
  final case class RotateL(deg: Int) extends Command
  final case class RotateR(deg: Int) extends Command

  final case class Ferry(dir: Dir, pos: Pos) {
    def apply(c: Command): Ferry =
      c match {
        case Forward(dist) => Ferry(dir, pos.add(dir, dist))
        case Move(moveDir, dist) => Ferry(dir, pos.add(moveDir, dist))
        case RotateL(deg) => Ferry(dir.rotateL(deg), pos)
        case RotateR(deg) => Ferry(dir.rotateR(deg), pos)
      }
  }

  final case class Follower(pos: Pos, waypoint: Pos) {
    def rot(deg: Int): (Int, Int, Int, Int) = {
      import Math._
      val rad = toRadians(deg)
      (cos(rad).toInt, -sin(rad).toInt,
        sin(rad).toInt, cos(rad).toInt)
    }
    def apply(c: Command): Follower =
      c match {
        case Forward(times) => Follower(pos + waypoint*times, waypoint)
        case Move(moveDir, dist) => Follower(pos, waypoint.add(moveDir, dist))
        case RotateL(deg) => Follower(pos, waypoint * rot(deg))
        case RotateR(deg) => Follower(pos, waypoint * rot(-deg))
      }
  }

  def parseCommands(it: Iterator[String]): List[Command] = {
    def parse(s: String): Option[Command] = {
      val (c, v) = s.splitAt(1)
      c match {
        case "L" => Some(RotateL(v.toInt))
        case "R" => Some(RotateR(v.toInt))
        case "F" => Some(Forward(v.toInt))
        case "N" => Some(Move(N, v.toInt))
        case "E" => Some(Move(E, v.toInt))
        case "S" => Some(Move(S, v.toInt))
        case "W" => Some(Move(W, v.toInt))
        case _ => None
      }
    }
    it.flatMap(parse).toList
  }

  def moveShip(seq: List[Command]): Int = {
    val last = seq.foldLeft(Ferry(E, Pos(0, 0))) { (f, c) =>
      f(c)
    }
    last.pos.dist
  }

  def moveShipWaypoint(seq: List[Command]): Int = {
    val last = seq.foldLeft(Follower(Pos(0, 0), Pos(10, 1))) { (f, c) =>
      println("Ship: " + f)
      f(c)
    }
    println("Last: " + last)
    last.pos.dist
  }

  val test1 = List(
    "F10",
    "N3",
    "F7",
    "R90",
    "F11"
  )

  def main(args: Array[String]): Unit = {
    require(args.length == 1)

    val commands = getLines(args(0), parseCommands)

    // Task 1
    println("Ship distance " + moveShip(commands))

    // Task 2
    println("Ship distance (Part II) " + moveShipWaypoint(commands))

  }

}
