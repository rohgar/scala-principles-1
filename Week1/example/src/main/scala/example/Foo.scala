package example

object Foo {
  def print = println("Hello world!!!!!")
}

abstract class Color(x: Int, y: Int) {
  def print(x: Int): String
  def color = "Blue"
  val color2 = "Red"
}

class A {
  
}

trait Colorful {
  def print(x: Int): String
  def color = "Blue"
  val color2 = "Red"
}