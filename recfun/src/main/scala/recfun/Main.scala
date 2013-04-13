package recfun
import common._
import scala.annotation.tailrec

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Pascal's Triangle
   * 1
   * 1 1
   * 1 2 1
   * 1 3 3 1
   * 1 4 6 4 1
   * 
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {

    if (c < 0 || r < 0)
      throw new IllegalArgumentException

    def recursivePascal(c: Int, r: Int): Int = {
      if (c == 0 || c == r) {
        // Left or Right edge
        1
      } else {
        // Inside triangle
        recursivePascal(c - 1, r - 1) + recursivePascal(c, r - 1)
      }
    }
    recursivePascal(c, r)
  }

  /**
   * Count number of braces
   * Make sure that no unbalanced )
   * 
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    @tailrec
    def balance(curr: Int, chars: List[Char]): Boolean = {
      if (chars.isEmpty) {
        curr == 0
      } else {
        val ch = chars.head
        if (ch == '(')
          balance(curr + 1, chars.tail)
        else if (ch == ')') {
          if (curr == 0)
            false
          else
            balance(curr - 1, chars.tail)
        } else
          balance(curr, chars.tail)
      }
    }
    balance(0, chars);
  }

  /**
   * Count change branches in different ways
   * 
   * If there are no possible coins then 0
   * A coin may be used repeatedly and that has to be combined with 
   * solutions where the coin is not there
   * 
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {

    if (coins.isEmpty)
      0
    else {
      val coin = coins.head
      val diff = money - coin
      // Solutions without this coin
      val other = countChange(money, coins.tail)
      if (diff == 0) {
        // Got a solution with this coin along with others
        1 + other
      } else if (diff > 0) {
        // Applying this coin multiple time may work
        countChange(diff, coins) + other
      } else {
        // Applying coin caused negative money
        other
      }
    }
  }
}
