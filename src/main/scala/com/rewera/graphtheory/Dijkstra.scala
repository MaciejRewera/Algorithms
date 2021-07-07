package com.rewera.graphtheory

import scala.collection.mutable

object Dijkstra {

  private case class Edge(targetNode: Int, weight: Int) {
    def copyWithChangedWeightSign: Edge = this.copy(weight = this.weight * -1)
  }

  private type WeightedGraph = Map[Int, Seq[Edge]]

  private val theGraph: WeightedGraph = Map(
    0 -> Seq(Edge(1, 4), Edge(2, 1)),
    1 -> Seq(Edge(3, 1)),
    2 -> Seq(Edge(1, 2), Edge(3, 5)),
    3 -> Seq(Edge(4, 3)),
    4 -> Seq()
  )

  private val theGraph_2: WeightedGraph = Map(
    0 -> Seq(Edge(1, 3), Edge(2, 6)),
    1 -> Seq(Edge(2, 4), Edge(3, 4), Edge(4, 11)),
    2 -> Seq(Edge(3, 8), Edge(6, 11)),
    3 -> Seq(Edge(4, 1), Edge(5, 5), Edge(6, 2)),
    4 -> Seq(Edge(7, 9)),
    5 -> Seq(Edge(7, 1)),
    6 -> Seq(Edge(7, 2)),
    7 -> Seq()
  )

  def main(args: Array[String]): Unit = {
    println(dijkstra(theGraph, 0).mkString("[", ", ", "]"))
    println(dijkstraOptimised(theGraph, 0, 4).mkString("[", ", ", "]"))
    println(dijkstra(theGraph_2, 0).mkString("[", ", ", "]"))
    println(dijkstraOptimised(theGraph_2, 0, 7).mkString("[", ", ", "]"))
    println(dijkstraOptimised(theGraph_2, 0, 2).mkString("[", ", ", "]"))

    println(dijkstraPath(theGraph, 0, 4).mkString("[", ", ", "]"))
    println(dijkstraPath(theGraph_2, 0, 7).mkString("[", ", ", "]"))

    println()
    dijkstraTracked(theGraph, 0).foreach(println)
    println()
    dijkstraTracked(theGraph_2, 0).foreach(println)
  }

  def dijkstra(graph: WeightedGraph, fromNode: Int): Seq[Int] = {
    val visited = Array.fill(graph.size)(false)
    val queue = new mutable.PriorityQueue()(Ordering.by[(Int, Int), Int](_._2).reverse)
    val distances = Array.fill[Option[Int]](graph.size)(None)
    distances(fromNode) = Some(0)
    queue.enqueue((fromNode, 0))

    while (queue.nonEmpty) {
      val (currNode, currNodeDist) = queue.dequeue()
      visited(currNode) = true

      if (distances(currNode).exists(_ >= currNodeDist)) {

        for (edge <- graph(currNode)) {
          if (!visited(edge.targetNode)) {

            val newDist = currNodeDist + edge.weight
            if (!distances(edge.targetNode).exists(_ < newDist)) {
              distances(edge.targetNode) = Some(newDist)
              queue.enqueue((edge.targetNode, newDist))
            }
          }
        }
      }
    }

    distances.flatten.toSeq
  }

  def dijkstraOptimised(graph: WeightedGraph, fromNode: Int, toNode: Int): Seq[Int] = {
    val visited = Array.fill(graph.size)(false)
    val queue = new mutable.PriorityQueue()(Ordering.by[(Int, Int), Int](_._2).reverse)
    val distances = Array.fill[Option[Int]](graph.size)(None)
    distances(fromNode) = Some(0)
    queue.enqueue((fromNode, 0))

    while (queue.nonEmpty) {
      val (currNode, currNodeDist) = queue.dequeue()
      visited(currNode) = true

      if (distances(currNode).exists(_ >= currNodeDist)) {

        for (edge <- graph(currNode)) {
          if (!visited(edge.targetNode)) {

            val newDist = currNodeDist + edge.weight
            if (!distances(edge.targetNode).exists(_ < newDist)) {
              distances(edge.targetNode) = Some(newDist)
              queue.enqueue((edge.targetNode, newDist))
            }

            if (currNode == toNode)
              return distances.flatten.toSeq
          }
        }
      }
    }

    distances.flatten.toSeq
  }

  def dijkstraPath(graph: WeightedGraph, fromNode: Int, toNode: Int): Seq[Int] = {
    val visited = Array.fill(graph.size)(false)
    val distances = Array.fill[Option[Int]](graph.size)(None)
    val previousNodes = Array.fill[Option[Int]](graph.size)(None)
    val queue = new mutable.PriorityQueue()(Ordering.by[(Int, Int), Int](_._2).reverse)

    distances(fromNode) = Some(0)
    queue.enqueue((fromNode, 0))

    while (queue.nonEmpty) {
      val (currNode, currNodeDist) = queue.dequeue()
      visited(currNode) = true

      if (distances(currNode).exists(_ >= currNodeDist)) {

        for (edge <- graph(currNode)) {
          if (!visited(edge.targetNode)) {

            val newDist = currNodeDist + edge.weight
            if (!distances(edge.targetNode).exists(_ < newDist)) {
              distances(edge.targetNode) = Some(newDist)
              previousNodes(edge.targetNode) = Some(currNode)
              queue.enqueue((edge.targetNode, newDist))
            }

            if (currNode == toNode)
              return reconstructPath(toNode).reverse
          }
        }
      }
    }

    def reconstructPath(currNode: Int): Seq[Int] = previousNodes(currNode) match {
      case None               => currNode +: Nil
      case Some(previousNode) => currNode +: reconstructPath(previousNode)
    }

    reconstructPath(toNode).reverse
  }

  case class Path(nodes: Seq[Int], totalWeight: Int)

  def dijkstraTracked(graph: WeightedGraph, fromNode: Int): Seq[Path] = {
    val visited = Array.fill(graph.size)(false)
    val paths = Array.fill[Option[Path]](graph.size)(None)
    val queue = new mutable.PriorityQueue()(Ordering.by[(Int, Int), Int](_._2).reverse)

    paths(fromNode) = Some(Path(Seq(fromNode), 0))
    queue.enqueue((fromNode, 0))

    while (queue.nonEmpty) {
      val (currNode, currNodeDist) = queue.dequeue()
      visited(currNode) = true

      if (paths(currNode).exists(_.totalWeight >= currNodeDist)) {

        for (edge <- graph(currNode)) {
          if (!visited(edge.targetNode)) {

            val newPathOpt = paths(currNode).map { path =>
              Path(edge.targetNode +: path.nodes, path.totalWeight + edge.weight)
            }

            for (newPath <- newPathOpt if !paths(edge.targetNode).exists(_.totalWeight < newPath.totalWeight)) {
              paths(edge.targetNode) = newPathOpt
              queue.enqueue((edge.targetNode, newPath.totalWeight))
            }
          }
        }
      }
    }

    paths.map { case Some(path) => path.copy(nodes = path.nodes.reverse) }
  }

}
