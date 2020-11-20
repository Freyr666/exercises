package example

object Hello extends Greeting with Rexp with App {
  testRegex()
  println(greeting)
}

trait Rexp {
  import regex.Regex

  def testRegex() {
    val r = Regex("((A*B|AC)*D)")
    println("Regex(\"((A*B|AC)*D)\").matches(\"AAAABACD\") = " + r.matches("AAAABACD"))
  }
}

trait Greeting {
  import Formatting._
  lazy val greeting: String = tp1('!')
}
