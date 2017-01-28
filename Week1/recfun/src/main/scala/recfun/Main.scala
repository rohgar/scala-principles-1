package recfun

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
   * Exercise 1
   * R/C 0 1 2 3 4
   * 0   1
   * 1   1 1
   * 2   1 2 1
   * 3   1 3 3 1
   * 4   1 4 6 4 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   * chars.drop(1) can be used inplace of chars.tail
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceList(chars: List[Char], count: Int): Boolean = { // positive count open (
      if (chars.isEmpty) if (count == 0) true else false
      else if (chars.head == '(') balanceList(chars.tail, count + 1)
      else if (chars.head == ')') {
        if (count - 1 >= 0) balanceList(chars.tail, count - 1)
        else false
      } else balanceList(chars.tail, count)
    }
    balanceList(chars, 0)
  }

  /**
   * Exercise 3
   * Reference: http://stackoverflow.com/questions/12629721/coin-change-algorithm-in-scala-using-recursion
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    // we recurse over money by reducing it. When we get exact 0, we have successfully made full change. Hence return 1 as we count this attempt
    // Also sequence here matters.
    if (money < 0) 0
    else if (money == 0) 1
    else if (coins.isEmpty) 0
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
  
}
