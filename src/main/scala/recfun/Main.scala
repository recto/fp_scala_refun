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
   */
    def pascal(c: Int, r: Int): Int = {
      if (c == 0 || c == r) 1
      else {
        pascal(c - 1, r - 1) + pascal(c, r  - 1)
      }
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def par(chars: List[Char], numOpens: Int): Boolean = {
        if (chars.isEmpty) {
          numOpens == 0
        } else {
          val h = chars.head
          val n = h match {
            case '(' => numOpens + 1
            case ')' => numOpens - 1
            case _ => numOpens
          }
          if (n >= 0) par(chars.tail, n)
          else false
        }
      }
      par(chars, 0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      def countChange(money:Int, coins: List[Int], count:Int): Int = {
        if (money < 0) count
        else {
          if (coins.isEmpty)
            if (money == 0) count + 1 else count
          else
            countChange(money - coins.head, coins, count) + countChange(money, coins.tail, count)
        }
      }
      countChange(money, coins, 0)
    }
  }
