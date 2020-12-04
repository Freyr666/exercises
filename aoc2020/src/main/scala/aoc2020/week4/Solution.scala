package aoc2020.week4

import aoc2020.utils
import cats.effect._

object Solution extends IOApp {

  final case class Passport(
    byr: Int,
    iyr: Int,
    eyr: Int,
    hgt: String,
    hcl: String,
    ecl: String,
    pid: String,
    cid: Option[String]
  ) {

    def properEyes: Boolean =
      ecl match {
        case "amb"
           | "blu"
           | "brn"
           | "gry"
           | "grn"
           | "hzl"
           | "oth" => true
        case _ => false
      }

    def properHeight: Boolean = {
      val height = hgt.dropRight(2).toIntOption
      (hgt.takeRight(2), height) match {
        case ("cm", Some(height)) =>
          height >= 150 && height <= 193
        case ("in", Some(height)) =>
          height >= 59 && height <= 76
        case _ => false
      }
    }

    def properPid: Boolean = {
      pid.length() == 9 &&
      pid.forall(_.isDigit)
    }

    def properHcl: Boolean = {
      hcl.length() == 7 &&
      hcl(0) == '#' &&
      hcl.takeRight(6).forall { char =>
        char.isDigit || (char >= 'a' && char <= 'f')
      }
    }

    def valid: Boolean = {
      byr >= 1920 && byr <= 2002 &&
      iyr >= 2010 && iyr <= 2020 &&
      eyr >= 2020 && eyr <= 2030 &&
      properHeight &&
      properEyes &&
      properPid &&
      properHcl
    }

  }

  object Passport {
    def apply(data: List[String]): Option[Passport] = {
      val map = data
        .flatMap(_.split(" +"))
        .map { s =>
          val Array(k, v) = s.split(':')
          k -> v
        }
        .foldLeft(Map.empty : Map[String, String])(_ + _)

      for {
        byr <- map.get("byr").map(_.toInt)
        iyr <- map.get("iyr").map(_.toInt)
        eyr <- map.get("eyr").map(_.toInt)
        hgt <- map.get("hgt")
        hcl <- map.get("hcl")
        ecl <- map.get("ecl")
        pid <- map.get("pid")
        cid = map.get("cid")
      } yield Passport(byr, iyr, eyr, hgt, hcl, ecl, pid, cid)
    }

  }

  def splitLines(it: Iterator[String]): List[List[String]] = {
    val (result, last) =
      it.foldLeft[(List[List[String]], List[String])]((Nil, Nil)) {
        case ((result, cur), "") => (cur::result, Nil)
        case ((result, cur), s) => (result, s::cur)
      }
    if (last.isEmpty) result
    else last::result
  }

  def run(args: List[String]): IO[ExitCode] = {
    require(args.nonEmpty)

    val data = utils.source(args.head)
    for {
      passports <- data.use(src => IO(splitLines(src
        .getLines())
        .flatMap(Passport(_))))
      _ <- IO(println("Number of passports: " + passports.length))
      _ <- IO(println("Number of valid passports: " + passports.count(_.valid)))
    } yield ExitCode.Success
  }

}
