package regex

import scala.collection.mutable

final case class Regex(rexp: String) {

  private val statesNum = rexp.length()
  private val re = rexp.toCharArray()
  private val epsilon = buildEpsilonGraph(rexp)

  def matches(text: String): Boolean = {
    var pc = Set.empty[Int]
    var dfs = DirectedDFS.single(epsilon, 0) // States reachable through epsilon-transition

    for (i <- 0 until epsilon.vertices)
      if (dfs.marked(i))
        pc += i

    for (i <- 0 until text.length()) {
      var mtch = Set.empty[Int]
      for (v <- pc) {
        if (v != statesNum)
          if (re(v) == text.charAt(i) || re(v) == '.')
            mtch += v + 1
      }

      dfs = DirectedDFS(epsilon, mtch)
      pc = Set.empty[Int]
      for (v <- 0 until epsilon.vertices)
        if (dfs.marked(v))
          pc += v
    }
    pc.exists(_ == statesNum)
  }

  private def buildEpsilonGraph(rexp: String): Digraph = {
    val g = Digraph(statesNum + 1)
    val ops = mutable.Stack.empty[Int]

    for (i <- 0 until statesNum) {
      var lpar = i

      // Par case
      if (re(i) == '(' || re(i) == '|')
        ops.push(i)
      else if (re(i) == ')') {
        val or = ops.pop()
        if (re(or) == '|') {
          lpar = ops.pop() // TODO allow multiple ORs as in (A|B|C)
          g.addEdge(lpar, or+1)
          g.addEdge(or, i)
        } else {
          lpar = or
        }
      }

      // * case
      if (i < statesNum - 1 && re(i+1) == '*') {
        g.addEdge(lpar, i+1)
        g.addEdge(i+1, lpar)
      }

      if (re(i) == '(' || re(i) == '*' || re(i) == ')')
        g.addEdge(i, i+1)

    }
    g
  }

}

protected final case class Digraph(v: Int) {

  val a: Array[List[Int]] =
    new Array[Vector[Int]](v).map(_ => Nil)

  def addEdge(i: Int, j: Int): Unit = {
    if (! a(i).exists(_ == j))
      a(i) = j::a(i)
  }

  def adj(i: Int): Iterable[Int] =
    a(i)

  def vertices: Int = v

  def edges: Int =
    a.foldLeft(0)((acc, l) => acc + l.length)

  def reverse: Digraph = {
    val res = Digraph(v)

    for (i <- 0 until v) {
      a(i).foreach(j => res.addEdge(j, i))
    }

    res
  }

}

protected case class DirectedDFS(g: Digraph, srcs: Iterable[Int]) {

  private val marks: Array[Boolean] = new Array[Boolean](g.vertices)
  for (s <- srcs)
    dfs(g, s)

  def marked(v: Int): Boolean =
    marks(v)

  private def dfs(g: Digraph, v: Int) {
    marks(v) = true
    for (w <- g.adj(v))
      if (! marks(w))
        dfs(g, w)
  }

}

protected object DirectedDFS {

  def single(g: Digraph, src: Int) =
    DirectedDFS(g, Seq(src))

}
