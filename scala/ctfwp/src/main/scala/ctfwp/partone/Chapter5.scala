package org.ctfwp.partone

object Chapter5 {

  def i(n: Int) = n
  def j(n: Boolean) = if (n) 0 else 1
  def m(e: Either[Int, Boolean]) =
    e match {
      case Left(n) => n
      case Right(b) => if (b) 0 else 1
    }

  val im : Int => Int = m _ compose (Left(_))
  val jm : Boolean => Int = m _ compose (Right(_))

  def i1(n: Int) =
    if (n < 0) n
    else n + 2
  def j1(n: Boolean) =
    if (n) 0 else 1
  def m1(e: Int): Either[Int, Boolean] =
    e match {
      case _ if e < 0 => Left(e)
      case 0 => Right(true)
      case 1 => Right(false)
      case _ => Left(e)
    }


  val im1 : Int => Either[Int, Boolean] = // Same as Left
    m1 _ compose i1 _
  val jm1 : Boolean => Either[Int, Boolean] = // Same as Right
    m1 _ compose j1 _

}
