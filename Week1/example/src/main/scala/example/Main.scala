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
  
  val time1 = time{Factorial.factorial(1111)}
  val time2 = time{Factorial.tailRecursiveFactorial(1111)}

}