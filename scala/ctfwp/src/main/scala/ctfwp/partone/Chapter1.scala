package org.ctfwp.partone

object Chapter1 {

  def id[A](a: A) = a

  implicit class CompositionSyntax[B, C](g: B => C) {
    def %[A](f: A => B): A => C =
      x => g(f(x))
  }

  def compTest[A, B](f: A => B, x: A): Unit = {
    assert((f % identity[A])(x) == f(x))
    assert((identity[B] _ % f)(x) == f(x))
  }

}
