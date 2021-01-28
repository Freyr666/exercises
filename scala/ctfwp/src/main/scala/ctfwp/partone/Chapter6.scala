package org.ctfwp.partone

object Chapter6 {

  // Option-Either isomorphism
  def optionToEither[A](o: Option[A]): Either[Unit, A] =
    o match {
      case None => Left(())
      case Some(x) => Right(x)
    }
  def eitherToOption[A](e: Either[Unit, A]): Option[A] =
    e match {
      case Left(_) => None
      case Right(x) => Some(x)
    }

  // Area as virtual function
  trait Shape {
    def area: Double
    def circ: Double
  }

  class Circle(r: Double) extends Shape {
    def area: Double =
      Math.PI * r * r
    def circ: Double =
      2.0 * Math.PI * r
  }

  class Rect(d: Double, h: Double) extends Shape {
    def area: Double =
      d * h
    def circ: Double =
      2.0 * (d + h)
  }

  class Square(side: Double) extends Rect(side, side)

  // a + a = 2 * a

  def sumToProd[A](sum: Either[A, A]): (Boolean, A) =
    sum match {
      case Left(x) => (false, x)
      case Right(x) => (true, x)
    }

  def prodToSum[A](prod: (Boolean, A)): Either[A, A] =
    if (prod._1) Right(prod._2)
    else Left(prod._2)

  def `test: prodToSum compose sumToProd = identity`
    [A](x: Either[A, A]): Unit = {
    val f: Either[A,A]=>Either[A,A] =
      prodToSum[A] _ compose sumToProd _
    assert(f(x) == x)
  }

}
