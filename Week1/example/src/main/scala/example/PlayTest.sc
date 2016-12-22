package example

object PlayTest {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  class PolyNom(terms0: Map[Int, Double]) {
    val terms = terms0 withDefaultValue 0.0 // field = param withDefaultValue 0.0
    def this(args: (Int, Double)*) = this(args.toMap)
    def +(other: PolyNom) = new PolyNom(terms ++ (other.terms map adjust))
    def adjust(term: (Int, Double)): (Int, Double) = {
      val (exp, coeff) = term
      // pattern match is now not needed now we don't need to test whether the given terms contain the given exponent or not
      exp -> (coeff + terms(exp))
    }
    override def toString() = {
      // (for( (exp, coeff) <- terms ) yield exp + "X^" + coeff) mkString "+"  // This prints fine, but in random order.
      (for ((exp, coeff) <- terms.toList.sorted.reverse) yield exp + "X^" + coeff) mkString " + "
    }
  }

  // val p1 = new PolyNom(Map(0 -> 5, 1 -> -2, 3 -> 1))
  val p2 = new PolyNom(0 -> 5, 1 -> -2, 3 -> 1)   //> p2  : example.PlayTest.PolyNom = 3X^1.0 + 1X^-2.0 + 0X^5.0
  val p3 = new PolyNom((0, 5), (1, -2), (3, 1))   //> p3  : example.PlayTest.PolyNom = 3X^1.0 + 1X^-2.0 + 0X^5.0

  val a = (3 -> 2)                                //> a  : (Int, Int) = (3,2)

  val capitalOfCountry = Map("US" -> "Washington", "Switzerland" -> "Bern")
                                                  //> capitalOfCountry  : scala.collection.immutable.Map[String,String] = Map(US 
                                                  //| -> Washington, Switzerland -> Bern)
  val countryOfCapital = capitalOfCountry  map {
    case (x, y) => (y, x)
  }                                               //> countryOfCapital  : scala.collection.immutable.Map[String,String] = Map(Was
                                                  //| hington -> US, Bern -> Switzerland)
                                                  

}