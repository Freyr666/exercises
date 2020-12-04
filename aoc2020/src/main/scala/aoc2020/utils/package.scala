package aoc2020

import cats.effect.{IO, Resource}
import scala.io.Source

package object utils {

  def source(f: String): Resource[IO, Source] =
    Resource.fromAutoCloseable(IO(Source.fromFile(f)))

}
