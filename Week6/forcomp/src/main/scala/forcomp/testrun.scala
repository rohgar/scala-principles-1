package forcomp

object testrun extends App {

  def combinations(occurrences: List[(Char, Int)]): List[List[(Char, Int)]] = {
    (occurrences.foldRight(List[List[(Char, Int)]](List())) {
      (elem, accumulatorListList) =>
        {
          val output = (for {
            accumulatorList <- accumulatorListList
            (char, num) = elem
            i <- 1 to num
          } yield (char, i) :: accumulatorList)
          println("For Output = " + output)
          println("Final Output = " + (accumulatorListList ::: output) )
          println("================================")
          accumulatorListList ::: output
        }
    })
  }

  combinations(List(('a', 2), ('b', 2)))
}