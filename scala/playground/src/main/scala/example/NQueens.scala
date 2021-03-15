package example

object NQueens {

  def conflicts(side: Int, placed: List[Int])(x: Int): Boolean =
    ! placed.exists { p =>
      (p % side == x % side) ||
      (p / side == x / side) ||
      (Math.abs(p % side - x % side) == (Math.abs(p / side - x / side)))
    }

  def nQueens(side: Int):  LazyList[List[Int]] = {
    def loop(acc: List[Int], n: Int): LazyList[List[Int]] =
      if (n == 0) LazyList(acc)
      else LazyList.from(0 until (side * side))
        .filter(conflicts(side, acc))
        .flatMap(x => loop(x::acc, n-1))
    loop(Nil, side)
  }

}
