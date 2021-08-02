package com.rewera.graphtheory

import com.rewera.graphtheory.WeightedDirectedGraph.{WeightedDirectedEdge => Edge, WeightedDirectedHalfEdge => HalfEdge}

object BellmanFord {

  private val theGraphEdges: WeightedDirectedGraph[Int, Int] = WeightedDirectedGraph(
    Seq(
      Edge(0, 1, 5),
      Edge(1, 2, 20),
      Edge(1, 5, 30),
      Edge(1, 6, 60),
      Edge(2, 3, 10),
      Edge(2, 4, 75),
      Edge(3, 2, -15),
      Edge(4, 9, 100),
      Edge(5, 4, 25),
      Edge(5, 6, 5),
      Edge(5, 8, 50),
      Edge(6, 7, -50),
      Edge(7, 8, -10)
    )
  )

  private val theGraph: WeightedDirectedGraph[Int, Int] = WeightedDirectedGraph(
    Map(
      0 -> Seq(HalfEdge(1, 5)),
      1 -> Seq(HalfEdge(2, 20), HalfEdge(5, 30), HalfEdge(6, 60)),
      2 -> Seq(HalfEdge(3, 10), HalfEdge(4, 75)),
      3 -> Seq(HalfEdge(2, -15)),
      4 -> Seq(HalfEdge(9, 100)),
      5 -> Seq(HalfEdge(4, 25), HalfEdge(6, 5), HalfEdge(8, 50)),
      6 -> Seq(HalfEdge(7, -50)),
      7 -> Seq(HalfEdge(8, -10))
    )
  )

  def main(args: Array[String]): Unit = {

    println(runBellmanFord(theGraphEdges, 0).mkString("[", ", ", "]"))
    println(runBellmanFord(theGraph, 0).mkString("[", ", ", "]"))
  }

  def runBellmanFord(graph: WeightedDirectedGraph[Int, Int], startNode: Int): Seq[Double] = {
    val verticesAmount = vertices(graph).size
    val distances = Array.fill(verticesAmount)(Double.PositiveInfinity)
    distances(startNode) = 0

    (0 until verticesAmount).foreach { _ =>
      graph.edgeList.foreach { edge =>
        val newDistance = distances(edge.fromNode) + edge.weight
        if (newDistance < distances(edge.toNode))
          distances(edge.toNode) = newDistance
      }
    }

    (0 until verticesAmount).foreach { _ =>
      graph.edgeList.foreach { edge =>
        val newDistance = distances(edge.fromNode) + edge.weight
        if (newDistance < distances(edge.toNode))
          distances(edge.toNode) = Double.NegativeInfinity
      }
    }

    distances
  }

  private def vertices[N, W](graph: WeightedDirectedGraph[N, W]): Set[N] =
    graph.edgeList.map(_.fromNode).toSet ++ graph.edgeList.map(_.toNode).toSet

}
