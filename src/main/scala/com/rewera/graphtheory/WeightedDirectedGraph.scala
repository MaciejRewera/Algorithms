package com.rewera.graphtheory

import com.rewera.graphtheory.WeightedDirectedGraph.{AdjacencyList, EdgeList, WeightedDirectedEdge}

case class WeightedDirectedGraph[N, W](adjacencyList: AdjacencyList[N, W]) {

  def edgeList: EdgeList[N, W] =
    adjacencyList.flatMap {
      case (fromNode, edges) =>
        edges.map(edge => WeightedDirectedEdge(fromNode, edge.toNode, edge.weight))
    }.toList

}

object WeightedDirectedGraph {
  type AdjacencyList[N, W] = Map[N, Seq[WeightedDirectedHalfEdge[N, W]]]
  type EdgeList[N, W] = Seq[WeightedDirectedEdge[N, W]]

  case class WeightedDirectedHalfEdge[N, W](toNode: N, weight: W)
  case class WeightedDirectedEdge[N, W](fromNode: N, toNode: N, weight: W)

  def apply[N, W](edgeList: EdgeList[N, W]): WeightedDirectedGraph[N, W] = {
    val adjacencyList =
      edgeList.groupMap(edge => edge.fromNode)(edge => WeightedDirectedHalfEdge(edge.toNode, edge.weight))
    WeightedDirectedGraph(adjacencyList)
  }
}
