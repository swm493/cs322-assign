package recfun

import scala.annotation.tailrec

object Main {
  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    require(c >= 0 && r >= 0, "c and r must be non-negative")
    require(c <= r, "c must be less than or equal to r")
    if (c == 0 || r == 0 || c == r)
      1
    else
      pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def iterateList(chars: List[Char], leftCount: Int, rightCount: Int): Boolean = {
      if (chars.isEmpty)
        leftCount == rightCount
      else if (chars.head == '(')
        iterateList(chars.tail, leftCount + 1, rightCount)
      else if (chars.head == ')')
        (leftCount > rightCount) && iterateList(chars.tail, leftCount, rightCount + 1)
      else
        iterateList(chars.tail, leftCount, rightCount)
    }

    iterateList(chars, 0, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (coins.isEmpty || money < 0)
      0
    else if (money == 0)
      1
    else
      countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
}
