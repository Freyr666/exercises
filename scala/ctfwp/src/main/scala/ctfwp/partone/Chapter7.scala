package org.ctfwp.partone

object Chapter7 {

  trait Functor[F[_]] {
    def fmap[A, B](f: A => B)(fa: F[A]): F[B]
  }

  implicit def function1Functor[R] = new Functor[({type F[A] = R => A})#F] {
    def fmap[A, B](f: A => B)(g: R => A): (R => B) =
      f compose g
  }

  // 1) fmap(identity)(g)
  //  - identity compose g
  //  - g
  //  - identity(g)
  // 2) fmap(g compose f) = fmap(g) compose fmap(f)
  //  - fmap(g compose f)(r)
  //  - g compose f compose r
  //  - g compose (fmap(f)(r))
  //  - fmap(g)(fmap(f)(r))

  // List
  // 1) fmap(identity) = identity
  //  - Nil
  //      fmap(identity)(Nil)
  //      Nil
  //      identity(Nil)
  //  - h::tl
  //      identity(h)::fmap(identity)(tl)
  //      h::fmap(identity)(tl)
  //      h::tl     (* by induction *)
  //      identity(h::tl)
}
