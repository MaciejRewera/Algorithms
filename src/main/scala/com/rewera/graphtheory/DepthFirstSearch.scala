package com.rewera.graphtheory

import scala.collection.mutable.{Seq => MSeq}

object DepthFirstSearch {

  private type Graph = Map[Int, Seq[Int]]

  private val theGraph: Graph = Map(
    1 -> Seq(3, 6, 7),
    2 -> Seq(8),
    3 -> Seq(1, 6, 7),
    4 -> Seq(5, 11, 16),
    5 -> Seq(4),
    6 -> Seq(1, 3, 13),
    7 -> Seq(1, 3),
    8 -> Seq(2, 15),
    9 -> Seq(),
    10 -> Seq(12),
    11 -> Seq(4, 16),
    12 -> Seq(10, 14, 17),
    13 -> Seq(6),
    14 -> Seq(12),
    15 -> Seq(8),
    16 -> Seq(4, 11),
    17 -> Seq(12),
  )

  def main(args: Array[String]): Unit = {

    dfs(theGraph)
  }

  private def dfs(graph: Graph): Unit = {
    val visited: MSeq[Boolean] = MSeq.fill(graph.size)(false)

    def dfs(at: Int): Seq[String] = {
      if (visited(at - 1)) return Seq.empty

      visited(at - 1) = true

      val neighbours = theGraph(at)

      at.toString +: neighbours.flatMap(dfs)
    }

    graph.keys.foreach { startNode =>
      val result = dfs(startNode).mkString(" ")
      if (result.nonEmpty)
        println(result)
    }
  }


}
