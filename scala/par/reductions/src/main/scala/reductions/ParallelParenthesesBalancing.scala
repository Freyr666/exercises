package reductions

import scala.annotation._
import org.scalameter._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime")
    println(s"speedup: ${seqtime.value / fjtime.value}")
  }
}

object ParallelParenthesesBalancing extends ParallelParenthesesBalancingInterface {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    def traverse(ind: Int, acc: Int): Boolean = acc match {
      case 0 if ind >= chars.length => true
      case _ if ind >= chars.length => false
      case 0 if chars(ind) == ')' => false
      case _ if chars(ind) == '(' => traverse(ind + 1, acc + 1)
      case n if n > 0 && chars(ind) == ')' => traverse(ind + 1, n - 1)
      case _ => traverse(ind + 1, acc)
    }
    traverse(0, 0)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, lpar: Int, rpar: Int): (Int, Int) = {
      if (idx == until) {
        (lpar, rpar)
      } else chars(idx) match {
        case '(' => traverse(idx + 1, until, lpar + 1, rpar)
        case ')' if lpar > 0 => traverse(idx + 1, until, lpar - 1, rpar)
        case ')' => traverse(idx + 1, until, lpar, rpar + 1)
        case _ => traverse(idx + 1, until, lpar, rpar)
      }
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      val len = until - from
      if (len <= threshold) traverse(from, until, 0, 0)
      else {
        val mid = from + len / 2
        val ((ll,lr), (rl,rr)) = parallel(reduce(from, mid), reduce(mid, until))
        if (rr < ll)
          (ll - rr + rl, lr)
        else
          (rl, rr - ll + lr)
      }
    }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
