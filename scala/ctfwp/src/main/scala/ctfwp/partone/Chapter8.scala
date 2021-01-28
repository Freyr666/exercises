package org.ctfwp.partone

import Chapter7._

object Chapter8 {

  trait Bifunctor[F[_, _]] {
    def bimap[A, B, C, D](g: A => C)(h: B => D): F[A, B] => F[C, D] =
      first(g) compose second(h)

    def first[A, B, C](g: A => C): F[A, B] => F[C, B] =
      bimap(g)(identity[B])

    def second[A, B, D](h: B => D): F[A, B] => F[A, D] =
      bimap(identity[A])(h)
  }

  implicit val tuple2Bifunctor = new Bifunctor[Tuple2] {
    override def bimap[A, B, C, D](f: A => C)(g: B => D): ((A, B)) => (C, D) = {
      case (x, y) => (f(x), g(y))
    }
  }

  implicit val eitherBifunctor = new Bifunctor[Either] {
    override def bimap[A, B, C, D](f: A=>C)(g: B=>D): Either[A, B]=>Either[C, D] = {
      case Left(x) => Left(f(x))
      case Right(x) => Right(g(x))
    }
  }

  type Id[A] = A

  case class BiComp[BF[_, _], FU[_], GU[_], A, B](v: BF[FU[A], GU[B]])

  implicit def bicompBiFunctor[
    BF[_, _], FU[_], GU[_]](
    implicit
      BF: Bifunctor[BF],
      FU: Functor[FU],
      GU: Functor[GU]) = {
    type BiCompAB[A, B] = BiComp[BF, FU, GU, A, B]
    new Bifunctor[BiCompAB] {
      override def bimap[A, B, C, D](f1: A => C)(f2: B => D):
          BiCompAB[A, B] => BiCompAB[C, D] = {
        case BiComp(x) =>
          BiComp(
            BF.bimap(FU.fmap(f1))(GU.fmap(f2))(x)
          )
      }
    }
  }

  // Functor implementation for Writer
  type Writer[A] = (A, String)
  object kleisli {
    implicit class KleisliOps[A, B](m1: A => Writer[B]) {
      def >=>[C](m2: B => Writer[C]): A => Writer[C] =
        x => {
          val (y, s1) = m1(x)
          val (z, s2) = m2(y)
          (z, s1 + s2)
        }
    }
  }
  def pure[A](x: A): Writer[A] = (x, "")

  implicit val writerFunctor: Functor[Writer] =
    new Functor[Writer] {
      import kleisli._
      def fmap[A, B](f: A => B)(fa: Writer[A]): Writer[B] = {
        val (x, s) = fa
        (f(x), s)
      }
    }

  // Option as Either Const Id composition

  type Const[A, B] = A
  type ConstUnit[A] = Const[Unit, A]

  implicit val constUnitFunctor: Functor[ConstUnit] =
    new Functor[ConstUnit] {
      def fmap[A, B](f: A => B)(x: ConstUnit[A]): ConstUnit[B] = x
    }
  implicit val idFunctor: Functor[Id] =
    new Functor[Id] {
      def fmap[A, B](f: A => B)(x: Id[A]): Id[B] = f(x)
    }

  val test1: BiComp[Either, ConstUnit, Id, Any, Int] = BiComp(Right[ConstUnit[Nothing], Id[Int]](42))
  val test2: BiComp[Either, ConstUnit, Id, Any, String] = BiComp(Left[ConstUnit[Nothing], Id[String]](()))


  // 1) Pair is a bifunctor
  case class Pair[A, B](a: A, b: B)
  implicit val pairBiFunctor: Bifunctor[Pair] = new Bifunctor[Pair] {
    override def bimap[A, B, C, D](g: A => C)(h: B => D): Pair[A,B] => Pair[C,D] = {
      case Pair(a, b) => Pair(g(a), h(b))
    }
  }

  // 2)
  type MyOpt[A] = Either[Const[Unit, A], Id[A]]
  def optionToMyOpt[A](x: Option[A]): MyOpt[A] = x match {
    case None => Left(())
    case Some(x) => Right(x)
  }
  def myOptToOption[A](x: MyOpt[A]): Option[A] = x match {
    case Left(()) => None
    case Right(x) => Some(x)
  }

}
