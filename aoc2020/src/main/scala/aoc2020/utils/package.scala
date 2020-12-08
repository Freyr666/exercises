package aoc2020

import cats.effect.{IO, Resource}
import scala.io.Source

package object utils {

  def source(f: String): Resource[IO, Source] =
    Resource.fromAutoCloseable(IO(Source.fromFile(f)))

  def getLines[B](file: String, f: Iterator[String]=>B): B = {
    val src = Source.fromFile(file)
    try {
      f(src.getLines())
    } finally {
      src.close()
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


}
