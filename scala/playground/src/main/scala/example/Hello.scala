package example

object Hello extends Greeting with App {
  println(greeting)
}

trait Greeting {
  import Formatting._
  lazy val greeting: String = tp1('!')
}
