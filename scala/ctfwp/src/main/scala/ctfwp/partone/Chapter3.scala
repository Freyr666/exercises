package org.ctfwp.partone

object Chapter3 {

  trait Monoid[M] {
    def empty: M
    def combine(m1: M, m2: M): M
  }

  object Monoid {
    implicit val stringMonoid: Monoid[String] =
      new Monoid[String] {
        def empty: String = ""
        def combine(m1: String, m2: String): String =
          m1 + m2
      }
  }

  def testMonoid[M: Monoid](m: M): Unit = {
    val mon = implicitly[Monoid[M]]
    import mon._
    assert(combine(empty, m) == combine(m, empty))
  }

  val modulo3: Monoid[Int] = new Monoid[Int] {
    def empty: Int = 0
    def combine(m1: Int, m2: Int): Int =
      (m1 + m2) % 3
  }

}
