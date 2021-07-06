package com.rewera.graphtheory

object TopologicalSort {

  private type Graph = Map[Int, Seq[Int]]
  private val theGraph: Graph = Map(
    0 -> Seq(3),
    1 -> Seq(3),
    2 -> Seq(0, 1),
    3 -> Seq(6, 7),
    4 -> Seq(0, 3, 5),
    5 -> Seq(10, 9),
    6 -> Seq(8),
    7 -> Seq(8, 9),
    8 -> Seq(11),
    9 -> Seq(11, 12),
    10 -> Seq(9),
    11 -> Seq(),
    12 -> Seq()
  )

  def main(args: Array[String]): Unit = {

    println(topologicalSort(theGraph).mkString("[", ", ", "]"))
    println(topologicalSort_2(theGraph).mkString("[", ", ", "]"))
    println(topologicalSort_3(theGraph).mkString("[", ", ", "]"))
    println(topologicalSort_4(theGraph).mkString("[", ", ", "]"))

  }

  def topologicalSort(graph: Graph): Seq[Int] = {
    val visited = Array.fill(graph.size)(false)
    val ordering = Array.fill(graph.size)(0)
    var i = graph.size - 1

    def dfs(currNode: Int): Int = {
      visited(currNode) = true
      val neighbours = graph(currNode)

      neighbours.foreach { neighbour =>
        if (!visited(neighbour))
          i = dfs(neighbour)
      }

      ordering(i) = currNode
      i - 1
    }

    graph.keys.toSeq.foreach { at =>
      if (!visited(at))
        i = dfs(at)
    }

    ordering.toSeq
  }

  def topologicalSort_2(graph: Graph) = {
    val visited = Array.fill(graph.size)(false)

    def dfs(currNode: Int): Seq[Int] = {
      if (visited(currNode)) Nil
      else {
        visited(currNode) = true

        val neighbours = graph(currNode)

        currNode +: neighbours.map(dfs).reverse.flatten
      }
    }

    graph.keys.toSeq.map(dfs).reverse.flatten
  }

  def topologicalSort_3(graph: Graph) = {
    val visited = Array.fill(graph.size)(false)

    def dfs(currNode: Int): Seq[Int] = {
      if (visited(currNode)) Nil
      else {
        visited(currNode) = true

        val neighbours = graph(currNode)

        currNode +: neighbours.map(dfs).foldLeft(Seq.empty[Int])((acc, elem) => elem ++ acc)
      }
    }

    graph.keys.map(dfs).foldLeft(Seq.empty[Int])((acc, elem) => elem ++ acc)
  }

  def topologicalSort_4(graph: Graph) = {
    val visited = Array.fill(graph.size)(false)
    var stack = Seq.empty[Int]

    def dfs(currNode: Int): Unit = {
      if (!visited(currNode)) {
        visited(currNode) = true

        val neighbours = graph(currNode)
        neighbours.foreach { neighbour =>
          dfs(neighbour)
        }

        stack = currNode +: stack
      }
    }

    graph.keys.foreach(dfs)
    stack
  }
}
