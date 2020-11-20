package example

//import zio._

object Tagless {

  trait Arith[Repr] {
    def lit(i: Int): Repr
    def neg(v: Repr): Repr
    def add(a: Repr, b: Repr): Repr
  }

  def lit[A](i: Int)(implicit a: Arith[A]): A = implicitly[Arith[A]].lit(i)
  def neg[A](e: A)(implicit a: Arith[A]): A = implicitly[Arith[A]].neg(e)
  def add[A](e1: A, e2: A)(implicit a: Arith[A]): A = implicitly[Arith[A]].add(e1, e2)

  implicit val arithInst: Arith[Int] = new Arith[Int] {
    def lit(i: Int) = i
    def neg(i: Int) = -i
    def add(a: Int, b: Int) = a + b
  }

  implicit val arithShow: Arith[String] = new Arith[String] {
    def lit(i: Int) = i.toString()
    def neg(s: String) = "(-" + s + ")"
    def add(a: String, b: String) = "(" + a + " + " + b + ")"
  }

  trait Mul[Repr] {
    def mul(a: Repr, b: Repr): Repr
  }

  def mul[A: Mul](a: A, b: A): A = implicitly[Mul[A]].mul(a, b)

  implicit val mulInst: Mul[Int] = new Mul[Int] {
    def mul(a: Int, b: Int) = a * b
  }

  implicit val mulShow: Mul[String] = new Mul[String] {
    def mul(a: String, b: String) = "(" + a + " * " + b + ")"
  }

  sealed trait Tree
  final case class Leaf(s: String) extends Tree
  final case class Node(s: String, ts: List[Tree]) extends Tree

  implicit val arithTree: Arith[Tree] = new Arith[Tree] {
    def lit(i: Int) = Node("lit", List(Leaf(i.toString())))
    def neg(v: Tree) = Node("neg", List(v))
    def add(a: Tree, b: Tree) = Node("add", List(a, b))
  }

  implicit val mulTree: Mul[Tree] = new Mul[Tree] {
    def mul(a: Tree, b: Tree) = Node("mul", List(a, b))
  }

  def fromTree[A](t: Tree)(implicit a: Arith[A]): Option[A] = {
    t match {
      case Node("lit", List(Leaf(n))) => n.toIntOption.map(a.lit)
      case Node("neg", List(t)) => fromTree(t)(a).map(a.neg)
      case Node("add", List(l, r)) => for {
        l <- fromTree(l)(a)
        r <- fromTree(r)(a)
      } yield(add(l, r))
      case _ => None
    }
  }

  implicit def arithDup[A, B](implicit a: Arith[A], b: Arith[B]) = new Arith[(A, B)] {
    def lit(x: Int) = (a.lit(x), b.lit(x))
    def neg(v: (A, B)) = (a.neg(v._1), b.neg(v._2))
    def add(l: (A, B), r: (A, B)) = (a.add(l._1, r._1), b.add(l._2, r._2))
  }

  def duplicate[A: Arith, B: Arith](v: (A, B)): (A, B) = v

  def checkConsume[A, B](f: A => B)(x: Option[A]) = x.map(f)

  def fromTreeArith[Repr: Arith](self: Tree => Option[Repr]): Tree => Option[Repr] = {
    case Node("lit", List(Leaf(n))) => n.toIntOption.map(implicitly[Arith[Repr]].lit)
    case Node("neg", List(n)) => self(n).map(implicitly[Arith[Repr]].neg)
    case Node("add", List(l,r)) => for {
      l <- self(l)
      r <- self(r)
    } yield(implicitly[Arith[Repr]].add(l, r))
    case _ => None
  }

  def fix[A, B](f: (A => B) => (A => B)): A => B = {
    case class W(wf: W => (A => B)) {
      def apply(w: W): A => B = wf(w)
    }
    val g: W => (A => B) = w => f(w(w))(_)
    g(W(g))
  }

  def fromTreeArithR[Repr: Arith]: Tree => Option[Repr] =
    fix((f: Tree => Option[Repr]) => fromTreeArith(f)(implicitly[Arith[Repr]]))

  sealed trait Ctx
  final case object Pos extends Ctx
  final case object Neg extends Ctx

  implicit def arithNorm[Repr](implicit m: Arith[Repr]): Arith[Ctx => Repr] = new Arith[Ctx => Repr] {
    def lit(n: Int) = {
      case Pos => m.lit(n)
      case Neg => m.neg(m.lit(n))
    }
    def neg(e: Ctx => Repr) = {
      case Pos => e(Neg)
      case Neg => e(Pos)
    }
    def add(l: Ctx => Repr, r: Ctx => Repr) = (ctx: Ctx) => m.add(l(ctx), r(ctx))
  }

  implicit def mulNorm[Repr](implicit m: Mul[Repr]) = new Mul[Ctx => Repr] {
    def mul(a: Ctx => Repr, b: Ctx => Repr) = {
      case Pos => m.mul(a(Pos), b(Pos))
      case Neg => m.mul(a(Pos), b(Neg))
    }
  }

  def pushNeg[A](f: Ctx => A): A = f(Pos)

}

object Lambda {

  trait Symantics[Repr[_,_]] {
    def int[H](i: Int): Repr[H, Int]
    def add[H](a: Repr[H,Int], b: Repr[H,Int]): Repr[H,Int]

    def z[H,A]: Repr[(A,H),A]
    def s[H,A,B](x: Repr[H, A]): Repr[(B,H),A]
    def lam[H,A,B](body: Repr[(A, H), B]): Repr[H, A => B]
    def app[H,A,B](f: Repr[H, A=>B], v: Repr[H, A]): Repr[H, B]
  }

  final case class R[H,A](unR: H => A)

  implicit val symanticsR = new Symantics[R] {
    def int[H](i: Int): R[H,Int] = R(_ => i)
    def add[H](a: R[H,Int], b: R[H,Int]): R[H,Int] = R((h: H) => a.unR(h) + b.unR(h))

    def z[H,A]: R[(A,H), A] = R{case (x,_) => x}
    def s[H,A,B](e: R[H,A]): R[(B,H),A] = R{case (_,h) => e.unR(h)}
    def lam[H,A,B](f: R[(A,H),B]): R[H, A=>B] = R(h => (x => f.unR((x,h))))
    def app[H,A,B](f: R[H,A=>B], x: R[H,A]): R[H,B] = R(h => f.unR(h)(x.unR(h)))
  }

  final case class S[H,A](unS: Int => String)

  implicit val symanticsS = new Symantics[S] {
    def int[H](i: Int): S[H,Int] = S(_ => i.toString())
    def add[H](a: S[H,Int], b: S[H,Int]): S[H,Int] = S(h =>
      "(" + a.unS(h) + " + " + b.unS(h) + ")")

    def z[H,A]: S[(A,H), A] = S(h => "var" + (h-1).toString())
    def s[H,A,B](e: S[H,A]): S[(B,H),A] = S(h => e.unS(h - 1))
    def lam[H,A,B](f: S[(A,H),B]): S[H, A=>B] = S(h => {
      val x = "var" + h.toString()
      "lambda " + x + " -> " + f.unS(h + 1)
    })
    def app[H,A,B](f: S[H,A=>B], x: S[H,A]): S[H,B] = S(h =>
      "(" + f.unS(h) + " " + x.unS(h) + ")"
    )
  }

  def td1[H,Repr[_,_]](implicit is: Symantics[Repr]): Repr[H,Int] =
    is.add(is.int(1), is.int(2))

  def td2[H,B,Repr[_,_]](implicit is: Symantics[Repr]): Repr[(Int,H), (Int => Int)] = {
    import is._
    lam(add(z[(Int,H),Int], s[(Int, H),Int,Int](z[H,Int])))
  }
}

object Lambda2 {

  trait Symantics[Repr[_]] {
    def int(i: Int): Repr[Int]
    def add(a: Repr[Int], b: Repr[Int]): Repr[Int]

    def lam[A,B](f: Repr[A] => Repr[B]): Repr[A => B]
    def app[A,B](f: Repr[A => B], arg: Repr[A]): Repr[B]
  }

  final case class R[A](unR: A)

  implicit val symanticsR = new Symantics[R] {
    def int(i: Int): R[Int] = R(i)
    def add(a: R[Int], b: R[Int]): R[Int] = R(a.unR + b.unR)

    def lam[A,B](f: R[A] => R[B]): R[A => B] = R(x => f(R(x)).unR)
    def app[A,B](f: R[A => B], arg: R[A]): R[B] = R(f.unR(arg.unR))
  }

  final case class S[A](unS: Int => String)

  implicit val symanticsS = new Symantics[S] {
    def int(i: Int): S[Int] = S(_ => i.toString())
    def add(a: S[Int], b: S[Int]): S[Int] = S(h =>
      "(" + a.unS(h) + " + " + b.unS(h) + ")"
    )

    def lam[A,B](f: S[A] => S[B]): S[A => B] = S(h => {
      val x = "var" + h.toString()
      "(lambda " + x + " -> " + f(S(_ => x)).unS(h + 1) + ")"
    })
    def app[A,B](f: S[A => B], arg: S[A]): S[B] = S(h =>
      "(" + f.unS(h) + " " + arg.unS(h) + ")"
    )
  }

  def eval[A](v: R[A]): A = v.unR
  def show[A](v: S[A]): String = v.unS(0)

  def th1[Repr[_]](implicit sem: Symantics[Repr]) = {
    import sem._
    add(int(1), int(2))
  }

  def th2[Repr[_]](implicit sem: Symantics[Repr]) = {
    import sem._
    lam(x => add(x, int(2)))
  }

  def th3[Repr[_]](implicit sem: Symantics[Repr]) = {
    import sem._
    lam((x: Repr[Int => Int]) => add(app(x, int(1)), int(2)))
  }

  trait MulSYM[Repr[_]] {
    def mul(a: Repr[Int], b: Repr[Int]): Repr[Int]
  }

  trait BoolSYM[Repr[_]] {
    def bool(v: Boolean): Repr[Boolean]
    def leq(a: Repr[Int], b: Repr[Int]): Repr[Boolean]
    def if_[A](c: Repr[Boolean], l: => Repr[A], r: => Repr[A]): Repr[A]
  }

  trait FixSYM[Repr[_]] {
    def fix[A](f: (=> Repr[A]) => Repr[A]): Repr[A]
  }

  implicit val mulSymR: MulSYM[R] = new MulSYM[R] {
    def mul(a: R[Int], b: R[Int]): R[Int] = R(a.unR * b.unR)
  }

  implicit val boolSymR: BoolSYM[R] = new BoolSYM[R] {
    def bool(v: Boolean): R[Boolean] = R(v)
    def leq(a: R[Int], b: R[Int]): R[Boolean] = R(a.unR <= b.unR)
    def if_[A](c: R[Boolean], l: => R[A], r: => R[A]): R[A] = {
      lazy val v = if (c.unR) { l.unR } else { r.unR }
      R(v)
    }
  }

  implicit val fixSymR: FixSYM[R] = new FixSYM[R] {
    def fix[A](f: (=> R[A]) => R[A]): R[A] = {
      def fx(f: (=> A) => A): A = f(fx(f))
      R(fx(x => f(R(x)).unR))
    }
  }

  def tpow[Repr[_]](implicit sem: Symantics[Repr],
    bsem: BoolSYM[Repr], msem: MulSYM[Repr], fsem: FixSYM[Repr]): Repr[Int=>Int=>Int] = {
    import sem._, bsem._, msem._, fsem._

    lam (x => fix (self => lam (n =>
      if_ (leq(n, int(0)),
        int(1),
        mul(x, app(self, (add(n, int(-1)))))))))
  }

  def tpow7[Repr[_]](implicit sem: Symantics[Repr],
    bsem: BoolSYM[Repr], msem: MulSYM[Repr], fsem: FixSYM[Repr]) = {
    import sem._, bsem._, msem._, fsem._

    lam ((x: Repr[Int]) => app(app(tpow(sem,bsem,msem,fsem), x), int(7)))
  }

  def tpow7_2[Repr[_]](implicit sem: Symantics[Repr],
    bsem: BoolSYM[Repr], msem: MulSYM[Repr], fsem: FixSYM[Repr]) = {
    import sem._, bsem._, msem._, fsem._

    app(tpow7(sem,bsem,msem,fsem), int(2))
  }

}

object Formatting {

  trait FormattingSpec[Repr[_,_]] {
    def lit[A](s: String): Repr[A,A]
    def int[A]: Repr[A, Int => A]
    def char[A]: Repr[A, Char => A]
    def concat[A,B,C](a: Repr[B,C], b: Repr[A,B]): Repr[A,C]
  }

  final case class FPr[A,B](f: ((String => A) => B))

  implicit class Concat[B,C,Repr[_,_]: FormattingSpec](v: Repr[B,C]) {
    def ++[A](that: Repr[A,B]): Repr[A,C] =
      implicitly[FormattingSpec[Repr]].concat(v, that)
  }

  implicit val formattingSpecFPr: FormattingSpec[FPr] = new FormattingSpec[FPr] {
    def lit[A](s: String): FPr[A,A] = FPr(k => k(s))
    def int[A]: FPr[A, Int => A] = FPr(k => x => k(x.toString()))
    def char[A]: FPr[A, Char => A] = FPr(k => x => k(x.toString()))
    def concat[A,B,C](a: FPr[B,C], b: FPr[A,B]): FPr[A,C] =
      FPr(k => a.f(sa => b.f(sb => k(sa + sb))))
  }

  def sprintf[B](fmt: FPr[String, B]): B = fmt.f(x => x)

  def tp1: Char => String = {
    import formattingSpecFPr._
    sprintf[Char => String](lit[Char => String]("Hello ") ++ lit("world") ++ char)
  }

}

object Linear {

  final case class F[A](a: A) // Variable
  sealed class U
  final case object Used extends U

  trait LSymantics[Repr[+_,+_,_]] { // EnvBefore, EnvAfter, Type
    def int[Hi](i: Int): Repr[Hi,Hi,Int]
    def add[Hi,Ho,H](a: Repr[Hi,H,Int], b: Repr[H,Ho,Int]): Repr[Hi,Ho,Int]

    def z[A,H]: Repr[(F[A],H), (U,H), A]
    def s[Hi,Ho,A](x: Repr[Hi,Ho,A]): Repr[(Any,Hi), (Any,Ho), A]
    def app[Hi,Ho,H,A,B](f: Repr[Hi,H,A=>B], arg: Repr[H,Ho,A]): Repr[Hi,Ho,B]
  }

  trait LineadL[Hi,Ho,Repr[+_,+_,_]] {
    def lam[A,B](body: Repr[(F[A],Hi), (U,Ho), B]): Repr[Hi,Ho,A=>B]
  }

  def tl1[Hi, Repr[+_,+_,_]: LSymantics] = {
    val sem = implicitly[LSymantics[Repr]]
    import sem._

    add(int[Hi](1), int[Hi](2))
  }

  /*
  def tl2o[H, Hi, Ho, Repr[+_,-_,_]](implicit sem: LSymantics[Repr], ll: LineadL[Hi,Ho,Repr]) = {
    import sem._
    import ll._

    lam(add(z, s(z)))
  }
   */

  def tl3[H, Hi, Ho, Repr[+_,+_,_]](implicit sem: LSymantics[Repr], ll: LineadL[Hi,Ho,Repr]) = {
    import sem._
    import ll._

    lam(add(app(z[Int=>Int,Hi], int[H](1)), int(2)))
  }

  /*
  def tl4[H, Hi, Ho, Repr[+_,+_,+_]](implicit sem: LSymantics[Repr], ll: LineadL[Hi,Ho,Repr]): Repr[Hi, Hi, ((Int => Int) => Int)] = {
    import sem._
    import ll._


    lam(lam(add(z,(s(z)))))
  }
   */

}
