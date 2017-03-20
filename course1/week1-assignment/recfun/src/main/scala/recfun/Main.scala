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
      if (r == 0) 1
      else if (r == 1) 1
      else if (c == 0) 1
      else if (c == r) 1
      else
        pascal(c-1, r-1) + pascal(c, r-1)
    }


  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      if (chars.isEmpty) true
      else
        balanceCheck(0, chars)

      def balanceCheck(count: Int, chars: List[Char]): Boolean = {
        if(chars.isEmpty)
          if(count == 0)
            return true
          else
            return false

        if (count < 0)
          return false

        if(chars.head.equals('('))
          balanceCheck(count+1, chars.tail)
        else if(chars.head.equals(')'))
          balanceCheck(count-1, chars.tail)
        else
          balanceCheck(count, chars.tail)
      }

    }

  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (money == 0) return 1
      if (money < 0) return 0
      if(coins.isEmpty) return 0

      countChange(money, coins.tail) + countChange(money-coins.head, coins)
    }
  }
