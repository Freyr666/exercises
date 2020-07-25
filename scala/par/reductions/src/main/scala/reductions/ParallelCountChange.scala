package reductions

import org.scalameter._

object ParallelCountChangeRunner {

  @volatile var seqResult = 0

  @volatile var parResult = 0

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 20,
    Key.exec.maxWarmupRuns -> 40,
    Key.exec.benchRuns -> 80,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val amount = 250
    val coins = List(1, 2, 5, 10, 20, 50)
    val seqtime = standardConfig measure {
      seqResult = ParallelCountChange.countChange(amount, coins)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential count time: $seqtime")

    def measureParallelCountChange(threshold: => ParallelCountChange.Threshold): Unit = try {
      val fjtime = standardConfig measure {
        parResult = ParallelCountChange.parCountChange(amount, coins, threshold)
      }
      println(s"parallel result = $parResult")
      println(s"parallel count time: $fjtime")
      println(s"speedup: ${seqtime.value / fjtime.value}")
    } catch {
      case e: NotImplementedError =>
        println("Not implemented.")
    }

    println("\n# Using moneyThreshold\n")
    measureParallelCountChange(ParallelCountChange.moneyThreshold(amount))
    println("\n# Using totalCoinsThreshold\n")
    measureParallelCountChange(ParallelCountChange.totalCoinsThreshold(coins.length))
    println("\n# Using combinedThreshold\n")
    measureParallelCountChange(ParallelCountChange.combinedThreshold(amount, coins))
  }
}

object ParallelCountChange extends ParallelCountChangeInterface {

  /** Returns the number of ways change can be made from the specified list of
   *  coins for the specified amount of money.
   */
  def countChange(money: Int, coins: List[Int]): Int = coins match {
    case Nil if money > 0 => 0
    case _ if money < 0 => 0
    case _ if money == 0 => 1
    case coin::tl if coin > money => countChange(money, tl)
    case coin::tl => countChange(money, tl) + countChange(money - coin, coin::tl)
  }

  type Threshold = (Int, List[Int]) => Boolean

  /** In parallel, counts the number of ways change can be made from the
   *  specified list of coins for the specified amount of money.
   */
  def parCountChange(money: Int, coins: List[Int], threshold: Threshold): Int = coins match {
    case Nil if money > 0 => 0
    case _ if money < 0 => 0
    case _ if money == 0 => 1
    case coin::tl if coin > money => parCountChange(money, tl, threshold)
    case coin::tl =>
      if (threshold(money, coins))
        countChange(money, tl) + countChange(money - coin, coin::tl)
      else {
        val (l, r) = parallel(parCountChange(money, tl, threshold), parCountChange(money - coin, coin::tl, threshold))
        r + l
      }
  }

  /** Threshold heuristic based on the starting money. */
  def moneyThreshold(startingMoney: Int): Threshold = {
    val th = (2 * startingMoney) / 3
    (money, coins) => money <= th
  }

  /** Threshold heuristic based on the total number of initial coins. */
  def totalCoinsThreshold(totalCoins: Int): Threshold = {
    val len = (2 * totalCoins) / 3
    (money, coins) => coins.length <= len
  }


  /** Threshold heuristic based on the starting money and the initial list of coins. */
  def combinedThreshold(startingMoney: Int, allCoins: List[Int]): Threshold = {
    val aggr = (startingMoney * allCoins.length) / 2
    (money, coins) => money * coins.length <= aggr
  }
}
