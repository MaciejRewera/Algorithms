package com.rewera.graphtheory

object SingleSourceShortestLongestPath {

  private case class Edge(targetNode: Int, weight: Int) {
    def copyWithChangedWeightSign: Edge = this.copy(weight = this.weight * -1)
  }

  private type WeightedGraph = Map[Int, Seq[Edge]]

  private val theGraph: WeightedGraph = Map(
    0 -> Seq(Edge(1, 3), Edge(2, 6)),
    1 -> Seq(Edge(2, 4), Edge(3, 4), Edge(4, 11)),
    2 -> Seq(Edge(3, 8), Edge(6, 11)),
    3 -> Seq(Edge(4, -4), Edge(5, 5), Edge(6, 2)),
    4 -> Seq(Edge(7, 9)),
    5 -> Seq(Edge(7, 1)),
    6 -> Seq(Edge(7, 2)),
    7 -> Seq()
  )

  def main(args: Array[String]): Unit = {
    println(dagShortestPaths(theGraph, 0).mkString("[", ", ", "]"))
    println(dagShortestPaths(theGraph, 3).mkString("[", ", ", "]"))
    println(dagLongestPaths(theGraph, 0).mkString("[", ", ", "]"))

    println()
    dagShortestPathsTracked(theGraph, 0).foreach(println)
  }

  def dagShortestPaths(graph: WeightedGraph, fromNode: Int) = {
    val topologicalOrder = TopologicalSort.topologicalSort_3(graph.map { case (k, v) => (k, v.map(_.targetNode)) })
    val distances = Array.fill[Option[Int]](graph.size)(None)
    distances(fromNode) = Some(0)

    topologicalOrder.foreach { currNode =>
      if (distances(currNode).nonEmpty) {
        val adjacentEdges = graph(currNode)

        for (edge <- adjacentEdges) {

          val newDist = distances(currNode).map(_ + edge.weight)

          if (distances(edge.targetNode).isEmpty) distances(edge.targetNode) = newDist
          else
            distances(edge.targetNode) = for {
              a <- distances(edge.targetNode)
              b <- newDist
            } yield Math.min(a, b)
        }
      }
    }

    distances.flatten.toSeq
  }

  def dagLongestPaths(graph: WeightedGraph, fromNode: Int) =
    dagShortestPaths(
      graph.map { case (k, neighbours) => k -> neighbours.map(edge => edge.copyWithChangedWeightSign) },
      fromNode
    ).map(_ * -1)

  case class Path(nodes: Seq[Int], totalWeight: Int)

  def dagShortestPathsTracked(graph: WeightedGraph, fromNode: Int) = {
    val topologicalOrder = TopologicalSort.topologicalSort_3(graph.map { case (k, v) => (k, v.map(_.targetNode)) })
    val paths = Array.fill[Option[Path]](graph.size)(None)
    paths(fromNode) = Some(Path(Seq(fromNode), 0))

    topologicalOrder.foreach { currNode =>
      paths(currNode).map { currNodePath =>
        val adjacentEdges = graph(currNode)
        for (edge <- adjacentEdges) {

          val newDist = currNodePath.totalWeight + edge.weight
          val newPathElem = Path(edge.targetNode +: currNodePath.nodes, newDist)

          if (paths(edge.targetNode).isEmpty || paths(edge.targetNode).exists(_.totalWeight > newDist))
            paths(edge.targetNode) = Some(newPathElem)
        }
      }
    }

    paths.map { case Some(path) => path.copy(nodes = path.nodes.reverse) }
  }

}
