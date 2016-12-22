package example

import scala.io.Source

object Phone {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  val in = Source.fromURL("https://raw.githubusercontent.com/rohitvg/scala-principles-1/master/resources/data/linuxwords.txt")
                                                  //> in  : scala.io.BufferedSource = non-empty iterator

  // In the list of words, filter such that for each word, each character is a letter (since our map only consists of letters and no special characters.
  var words = in.getLines.toList.filter(word => word.forall(chr => chr.isLetter))
                                                  //> words  : List[String] = List(P, Q, S, PA, PC, Aarhus, Aaron, Ababa, aback, a
                                                  //| baft, abandon, abandoned, abandoning, abandonment, abandons, abase, abased, 
                                                  //| abasement, abasements, abases, abash, abashed, abashes, abashing, abasing, a
                                                  //| bate, abated, abatement, abatements, abater, abates, abating, Abba, abbe, ab
                                                  //| bey, abbeys, abbot, abbots, Abbott, abbreviate, abbreviated, abbreviates, ab
                                                  //| breviating, abbreviation, abbreviations, Abby, abdomen, abdomens, abdominal,
                                                  //|  abduct, abducted, abduction, abductions, abductor, abductors, abducts, Abe,
                                                  //|  abed, Abel, Abelian, Abelson, Aberdeen, Abernathy, aberrant, aberration, ab
                                                  //| errations, abet, abets, abetted, abetter, abetting, abeyance, abhor, abhorre
                                                  //| d, abhorrent, abhorrer, abhorring, abhors, abide, abided, abides, abiding, A
                                                  //| bidjan, Abigail, Abilene, abilities, ability, abject, abjection, abjections,
                                                  //|  abjectly, abjectness, abjure, abjured, abjures, abjuring, ablate, ablated, 
                                                  //| ablates, ablating, ablat
                                                  //| Output exceeds cutoff limit.

  val mnemonics = Map('2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL", '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ")
                                                  //> mnemonics  : scala.collection.immutable.Map[Char,String] = Map(8 -> TUV, 4 -
                                                  //| > GHI, 9 -> WXYZ, 5 -> JKL, 6 -> MNO, 2 -> ABC, 7 -> PQRS, 3 -> DEF)

  // Map each letter to its digit
  val charCode: Map[Char, Char] = for (
    (digit, letters) <- mnemonics;
    letter <- letters
  ) yield (letter -> digit)                       //> charCode  : Map[Char,Char] = Map(E -> 3, X -> 9, N -> 6, T -> 8, Y -> 9, J -
                                                  //| > 5, U -> 8, F -> 3, A -> 2, M -> 6, I -> 4, G -> 4, V -> 8, Q -> 7, L -> 5,
                                                  //|  B -> 2, P -> 7, C -> 2, H -> 4, W -> 9, K -> 5, R -> 7, O -> 6, D -> 3, Z -
                                                  //| > 9, S -> 7)

  // Map a word to the digit string
  def wordCode(word: String): String = word.toUpperCase.map(charCode)
                                                  //> wordCode: (word: String)String

  // Map from digit string to respective possible words
  def numToWord: Map[String, Seq[String]] = {
    // withDefaultValue (Seq()) is used so that if user tries to encode digits the words for which are not contained in our list, we do not get an exception.
    words.groupBy(wordCode) withDefaultValue (Seq())
  }                                               //> numToWord: => Map[String,Seq[String]]

  // Function to encode a number as a list of all possible words
  def encode(numberStr: String): Set[List[String]] = {
    if (numberStr.isEmpty) Set(List())
    else {
      for {
        split <- 1 to numberStr.length
        word <- numToWord(numberStr.take(split))
        rest <- encode(numberStr.drop(split))
      } yield {
        word :: rest
      }
    }.toSet
  }                                               //> encode: (numberStr: String)Set[List[String]]

  // Test!
  //encode("225247386")
  encode("72")                                    //> res0: Set[List[String]] = Set(List(PA), List(PC))

}