package example

object Main extends App {

  // Test the lists function
  val nums: List[Int] = List(1, 2, 3, 4)
  println("\nMax = " + Lists.max(nums))
  println("Sum = " + Lists.sum(nums) + "\n")

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) + "ns")
    result
  }

  // test the factorial function
  val x = Factorial.factorial(9)
  println("Factorial x = " + x)

  val y = Factorial.tailRecursiveFactorial(9)
  println("Factorial y = " + y)

  val time1 = time { Factorial.factorial(2) }
  val time2 = time { Factorial.tailRecursiveFactorial(2) }

  def countChange(money: Int, coins: List[Int]): Int = {
    if (money < 0 || coins.isEmpty) 0
    // we recurse over money by reducing it. When we get exact 0, we have successfully made full change. Hence return 1 as we count this attempt
    else if (money == 0) 1 
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
  println("Count change = "+countChange(100, List(1,2)))
  
  example.Foo.print
  
  

}