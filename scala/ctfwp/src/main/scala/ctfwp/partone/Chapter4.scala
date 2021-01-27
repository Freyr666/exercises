package org.ctfwp.partone

object Chapter4 {

  def pure[A](x: A): Option[A] = Some(x)
  implicit class KleisliComposition[B, C](g: B => Option[C]) {
    def comp[A](f: A => Option[B]): A => Option[C] =
      (x: A) => f(x) match {
        case None => None
        case Some(res) => g(res)
      }
  }

  def safeReciprocal(x: Double): Option[Double] =
    if (x == 0) None
    else Some(1/x)

  def safeRoot(x: Double): Option[Double] =
    if (x < 0) None
    else Some(Math.sqrt(x))

  val safeRootReciprocal: Double => Option[Double] =
    safeRoot _ comp safeReciprocal

}
