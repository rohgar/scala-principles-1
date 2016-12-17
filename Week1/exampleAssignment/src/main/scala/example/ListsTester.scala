package example

object ListsTester extends App {
  val nums: List[Int] = List(1, 2, 3, 4)
  println("\nMax = "+Lists.max(nums))
  println("Sum = "+Lists.sum(nums)+"\n")
}