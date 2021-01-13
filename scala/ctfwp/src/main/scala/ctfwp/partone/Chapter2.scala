package org.ctfwp.partone

import scala.util._

object Chapter2 {

  def memoize[A, B](f: A => B): A => B = {
    import scala.collection.mutable.HashMap
    val table = new HashMap[A, B]

    (x: A) =>
      table.get(x) match {
        case Some(v) => v
        case None => {
          val v = f(x)
          table(x) = v
          v
        }
      }
  }

  val randomFromSeed: Int => Int =
    memoize { seed: Int =>
      val gen = new Random(seed)
      gen.nextInt()
    }

  val `all the bool functions`: Seq[Boolean=>Boolean] = Seq(
    _ => true,
    _ => false,
    x => !x,
    x => x
  )

}
