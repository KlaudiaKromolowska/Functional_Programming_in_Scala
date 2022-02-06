package recfun
import recfun.RecFunInterface
import scala.annotation.tailrec

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = if r==c || c==0 then 1 else pascal(c-1,r-1)+pascal(c,r-1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def ifBalanced(opened: Int, chars: List[Char]): Boolean = {
      if (opened <0) return false
      if (chars.isEmpty) opened == 0
      else if(chars.head=='(') ifBalanced(opened+1,chars.tail)
      else if(chars.head==')') ifBalanced(opened-1,chars.tail)
      else ifBalanced(opened, chars.tail)
    }
    ifBalanced(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money ==0) 1
    else if (money<0 ||coins.isEmpty) 0
    else countChange(money-coins.head, coins)+countChange(money,coins.tail)
  }
