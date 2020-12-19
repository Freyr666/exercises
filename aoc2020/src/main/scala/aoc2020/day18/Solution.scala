package aoc2020.day18

import aoc2020.utils._

object Solution {

  sealed trait Tok
  case object Add extends Tok
  case object Mul extends Tok
  case object LPar extends Tok
  case object RPar extends Tok
  final case class Lit(v: Long) extends Tok

  def tokenize(s: String): List[Tok] = {
    def loop(acc: List[Tok], off: Int): List[Tok] =
      if (off == s.length()) acc.reverse
      else (acc, s(off)) match {
        case (Lit(x)::tl, n) if n.isDigit =>
          loop(Lit((n - '0').toLong + 10*x)::tl, off+1)
        case (_, n) if n.isDigit =>
          loop(Lit((n - '0').toLong)::acc, off+1)
        case (_, '(') =>
          loop(LPar::acc, off+1)
        case (_, ')') =>
          loop(RPar::acc, off+1)
        case (_, '+') =>
          loop(Add::acc, off+1)
        case (_, '*') =>
          loop(Mul::acc, off+1)
        case (_, spc) if spc.isSpaceChar =>
          loop(acc, off+1)
        case _ => ???
      }
    loop(Nil, 0)
  }

  def noPrecEval(exps: List[List[Tok]]): Long = {

    def applyAll(opStack: List[Tok], stack: List[Long]): Long =
      (opStack, stack) match {
        case (Add::tl, a::b::rest) =>
          applyAll(tl, (a + b)::rest)
        case (Mul::tl, a::b::rest) =>
          applyAll(tl, (a * b)::rest)
        case (Nil, res::_) =>
          res
        case _ =>
          ???
      }

    def eval(exp: List[Tok], opStack: List[Tok], stack: List[Long]): (Long, List[Tok]) =
      exp match {
        case Nil =>
          (applyAll(opStack.reverse, stack.reverse), Nil)
        case RPar::tl =>
          (applyAll(opStack.reverse, stack.reverse), tl)
        case LPar::tl => {
          val (res, rest) = eval(tl, Nil, Nil)
          eval(rest, opStack, res::stack)
        }
        case Lit(x)::tl =>
          eval(tl, opStack, x::stack)
        case (o @ (Add | Mul))::tl  =>
          eval(tl, o::opStack, stack)
        case _ =>
          ???
      }

    exps
      .map(e => eval(e, Nil, Nil)._1)
      .foldLeft(0L) { (acc, res) => acc + res }
  }

  def precEval(exps: List[List[Tok]]): Long = {

    def prec(op: Tok): Int =
      op match {
        case Add => 1
        case Mul => 0
        case _ => -1
      }

    def apply(op: Tok, a: Long, b: Long): Long =
      op match {
        case Add => a + b
        case Mul => a * b
        case _ => ???
      }

    def eval(exp: List[Tok], ops: List[Tok], nums: List[Long]): (Long, List[Tok]) =
      (exp, ops, nums) match {
        case (Nil, Nil, res::_) =>
          (res, Nil)
        case (Nil, op::ops, a::b::nums) =>
          eval(exp, ops, apply(op, a, b)::nums)
        case (RPar::tl, Nil, res::_) =>
          (res, tl)
        case (RPar::_, op::ops, a::b::nums) =>
          eval(exp, ops, apply(op, a, b)::nums)
        case (Lit(x)::tl, _, _) =>
          eval(tl, ops, x::nums)
        case (LPar::tl, ops, nums) => {
          val (res, rest) = eval(tl, Nil, Nil)
          eval(rest, ops, res::nums)
        }
        case ((o @ (Add | Mul))::_, op::ops, a::b::nums) if prec(o) < prec(op) =>
          eval(exp, ops, apply(op, a, b)::nums)
        case ((o @ (Add | Mul))::tl, ops, nums) =>
          eval(tl, o::ops, nums)
        case _ =>
          ???
      }

    exps
      .map(e => eval(e, Nil, Nil)._1)
      .foldLeft(0L) { (acc, res) => acc + res }
  }

  def main(args: Array[String]) = {
    require(args.length == 1)

    val exps = getLines(args(0), _.map(tokenize).toList)

    // Task 1
    println("Sum of exps " + noPrecEval(exps))

    // Task 2
    println("Sum of exps (Task 2) " + precEval(exps))
  }

}
