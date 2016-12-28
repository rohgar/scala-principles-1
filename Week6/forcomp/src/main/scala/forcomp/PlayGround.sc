package forcomp

object PlayGround {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  type Word = String
  type Sentence = List[Word]
  type Occurrences = List[(Char, Int)]

  def wordOccurrences(w: Word) = (w.toLowerCase().groupBy(e => e)).map(e => (e._1, e._2.length)).toList.sorted
                                                  //> wordOccurrences: (w: forcomp.PlayGround.Word)List[(Char, Int)]

  def sentenceOccurrences(s: Sentence) = wordOccurrences(s.mkString(""))
                                                  //> sentenceOccurrences: (s: forcomp.PlayGround.Sentence)List[(Char, Int)]

  val dictionary: List[Word] = List("Ate", "tea", "eat", "tae")
                                                  //> dictionary  : List[forcomp.PlayGround.Word] = List(Ate, tea, eat, tae)
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = dictionary.groupBy(e => wordOccurrences(e))
                                                  //> dictionaryByOccurrences: => Map[forcomp.PlayGround.Occurrences,List[forcomp.
                                                  //| PlayGround.Word]]

  def wordAnagrams(word: Word): List[Word] = dictionaryByOccurrences(wordOccurrences(word))
                                                  //> wordAnagrams: (word: forcomp.PlayGround.Word)List[forcomp.PlayGround.Word]

  wordAnagrams("ate")                             //> res0: List[forcomp.PlayGround.Word] = List(Ate, tea, eat, tae)

  val a1 = List("Ate", "tea", "eat", "tae")       //> a1  : List[String] = List(Ate, tea, eat, tae)
  val a2 = List("1", "two")                       //> a2  : List[String] = List(1, two)
  a1 ::: a2                                       //> res1: List[String] = List(Ate, tea, eat, tae, 1, two)
  val a3 = a1 :: List("string")                   //> a3  : List[java.io.Serializable] = List(List(Ate, tea, eat, tae), string)
  val a4 = 1 :: 2 :: 3 :: Nil                     //> a4  : List[Int] = List(1, 2, 3)

  val list = List(('a', 2), ('b', 2))             //> list  : List[(Char, Int)] = List((a,2), (b,2))

  val list1 = for {
    (char, num) <- list;
    i <- 1 to num
  } yield (char, i)                               //> list1  : List[(Char, Int)] = List((a,1), (a,2), (b,1), (b,2))

  val t = List(2, 3, 4, 5)                        //> t  : List[Int] = List(2, 3, 4, 5)
  val tr = t.foldRight(0) { (n: Int, acc: Int) => n }
                                                  //> tr  : Int = 2
  val tl = t.foldLeft(0) { (acc: Int, n: Int) => n }
                                                  //> tl  : Int = 5
  t.foldLeft(0) { (acc: Int, n: Int) => n + acc } //> res2: Int = 14
  t.foldRight(0) { (n: Int, acc: Int) => n + acc }//> res3: Int = 14

  val listo = List(('a', 2), ('b', 2))            //> listo  : List[(Char, Int)] = List((a,2), (b,2))
  Map("first" -> 1, "second" -> 2).foldLeft(0) { case (a, (k, v)) => a + v }
                                                  //> res4: Int = 3
  for (
    (char, num) <- listo
  ) yield (char, num)                             //> res5: List[(Char, Int)] = List((a,2), (b,2))

  for (
    i <- 1 to 2;
    j <- 11 to 12
  ) yield i :: j :: Nil                           //> res6: scala.collection.immutable.IndexedSeq[List[Int]] = Vector(List(1, 11)
                                                  //| , List(1, 12), List(2, 11), List(2, 12))

  val first = listo.foldRight(List[Occurrences](List())) {
    case ((char, num), acc) =>
      (for (
        elem <- acc;
        i <- 0 to num
      ) yield (char, i) :: elem)
  }                                               //> first  : List[forcomp.PlayGround.Occurrences] = List(List((a,0), (b,0)), Li
                                                  //| st((a,1), (b,0)), List((a,2), (b,0)), List((a,0), (b,1)), List((a,1), (b,1)
                                                  //| ), List((a,2), (b,1)), List((a,0), (b,2)), List((a,1), (b,2)), List((a,2), 
                                                  //| (b,2)))
  val second = listo.foldRight(List[Occurrences](List())) {
    (elem, accumulatorListList) =>
      (for (
        accumulatorList <- accumulatorListList;
        (char, num) = elem;
        i <- 0 to num
      ) yield (char, i) :: accumulatorList)
  }                                               //> second  : List[forcomp.PlayGround.Occurrences] = List(List((a,0), (b,0)), L
                                                  //| ist((a,1), (b,0)), List((a,2), (b,0)), List((a,0), (b,1)), List((a,1), (b,1
                                                  //| )), List((a,2), (b,1)), List((a,0), (b,2)), List((a,1), (b,2)), List((a,2),
                                                  //|  (b,2)))
}