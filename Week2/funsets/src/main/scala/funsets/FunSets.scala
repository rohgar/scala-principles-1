package funsets

/**
 * 2. Purely Functional Sets.
 */
object FunSets {
  /**
   * We represent a set by its characteristic function, i.e.
   * its `contains` predicate.
   */
  type Set = Int => Boolean

  /**
   * Indicates whether a set contains a given element.
   */
  def contains(s: Set, elem: Int): Boolean = s(elem)

  /**
   * Returns the set of the one given element.
   * Based on the characteristic function, in the func that we return, take an Int and check if its the element of set
   */
  def singletonSet(elem: Int): Set = (x: Int) => x == elem

  /**
   * Returns the union of the two given sets,
   * the sets of all elements that are in either `s` or `t`.
   */
  def union(s: Set, t: Set): Set = (x: Int) => contains(s, x) || contains(t, x)

  /**
   * Returns the intersection of the two given sets,
   * the set of all elements that are both in `s` and `t`.
   */
  def intersect(s: Set, t: Set): Set = (x: Int) => contains(s, x) && contains(t, x)

  /**
   * Returns the difference of the two given sets,
   * the set of all elements of `s` that are not in `t`.
   */
  def diff(s: Set, t: Set): Set = (x: Int) => contains(s, x) && !contains(t, x)

  /**
   * Returns the subset of `s` for which `p` holds.
   */
  def filter(s: Set, p: Int => Boolean): Set = (x: Int) => contains(s, x) && p(x)

  /**
   * The bounds for `forall` and `exists` are +/- 1000.
   */
  val bound = 1000

  /**
   * Returns whether all bounded integers within `s` satisfy `p`.
   * i.e. if ALL the bounded integers within s satisfy p or not. So considering unbounded to be true.
   * Also since we are trying to find if all of the bounded integers within s satisfy p, keep iterating
   * until you find a false condition and then return. So if all of them satisfy control will go to the 
   * unbounded state and finally return true. Else it'll return false at some point in the middle.
   */
  def forall(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (a < -bound || a > bound) true        // unbounded
      else if (contains(s, a) && !p(a)) false
      else iter(a - 1)
    }
    iter(bound)
  }

  /**
   * Returns whether there exists a bounded integer within `s`
   * that satisfies `p`.
   * Our forAll funcion only returns false for !p(a), so pass the negation of that i.e. the 
   * new p(a) is !p(a). So the output will be false when the predicate is satisfied. So negate the
   * output as well, so that out putis true when predicate is satisfied. Also if no element satisfies
   * the predicate, i.e. control goes to the unbounded state, the output should be false. The negation of 
   * output takes care of this case.
   */
  def exists(s: Set, p: Int => Boolean): Boolean = !forall(s, x => !p(x))

  /**
   * Returns a set transformed by applying `f` to each element of `s`.
   * Similar to a singleton set, just make sure that the element is the transformed element from s
   * For any y, if there exists an element x in s that satisfies the condition f(x) equals y, then y is in new Set.
   */
  def map(s: Set, f: Int => Int): Set = (y: Int) => exists(s, x => f(x) == y)

  /**
   * Displays the contents of a set
   */
  def toString(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  /**
   * Prints the contents of a set on the console.
   */
  def printSet(s: Set) {
    println(toString(s))
  }
}
