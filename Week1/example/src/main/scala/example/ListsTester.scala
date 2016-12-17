package example

object ListsTester extends App {
  val nums: List[Int] = List(1, 2, 3, 4)
  print("=============================")
  println("Max = "+Lists.max(nums))
  println("Sum = "+Lists.sum(nums))
}