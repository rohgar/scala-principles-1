object playground {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  def times(chars: List[Char]): List[(Char, Int)] = (chars.groupBy(identity).mapValues(_.size)).toList
                                                  //> times: (chars: List[Char])List[(Char, Int)]

  val t = times("abaadabc".toList)                //> t  : List[(Char, Int)] = List((b,2), (d,1), (a,4), (c,1))
  val sortedFreqs = t.sortBy(pair => pair._2)     //> sortedFreqs  : List[(Char, Int)] = List((d,1), (c,1), (b,2), (a,4))
  val orderedLeafList = sortedFreqs.map(pair => pair._1)
                                                  //> orderedLeafList  : List[Char] = List(d, c, b, a)

  // def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = { freqs.sorted }
}