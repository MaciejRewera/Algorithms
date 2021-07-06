package com.rewera.graphtheory

import scala.collection.immutable.Queue
import scala.collection.mutable.{Queue => MQueue, Seq => MSeq}

object BreadthFirstSearch {

  private type Graph = Map[Int, Seq[Int]]

  private val theGraph: Graph = Map(
    0 -> Seq(2, 3, 7, 12),
    1 -> Seq(4),
    2 -> Seq(0, 3, 4),
    3 -> Seq(0, 2, 4, 9),
    4 -> Seq(1, 2, 3),
    5 -> Seq(7, 11),
    6 -> Seq(7, 8, 11),
    7 -> Seq(0, 5, 6),
    8 -> Seq(6, 9, 10, 12),
    9 -> Seq(3, 8),
    10 -> Seq(8),
    11 -> Seq(5, 6),
    12 -> Seq(0, 8)
  ).mapValues(_.sorted)

  def main(args: Array[String]): Unit = {
    breadthFirstSearch(theGraph, 0)
    println()
    breadthFirstSearchRec(theGraph, 0)
    println()
    breadthFirstSearchNoQueue(theGraph, 0)
    println()
    println()

    (1 to 12) foreach { destinationNode =>
      println(s"0 -> $destinationNode  : ${shortestPath(theGraph, 0, destinationNode).mkString("-")}")
    }

    println(s"1 -> 12  : ${shortestPath(theGraph, 1, 12).mkString("-")}")
    println(s"1 -> 1  : ${shortestPath(theGraph, 1, 1).mkString("-")}")
    println(s"1 -> 10  : ${shortestPath(theGraph, 1, 10).mkString("-")}")
    println(s"1 -> 5  : ${shortestPath(theGraph, 1, 5).mkString("-")}")
    println(s"1 -> 11  : ${shortestPath(theGraph, 1, 11).mkString("-")}")
    println(s"1 -> 13  : ${shortestPath(theGraph, 1, 13).mkString("-")}")
  }

  private def breadthFirstSearch(graph: Graph, startNode: Int): Unit = {
    val queue = MQueue.empty[Int]
    val visited: MSeq[Boolean] = MSeq.fill(graph.size)(false)
    queue.enqueue(startNode)

    while (queue.nonEmpty) {
      val currNode = queue.dequeue()
      visited(currNode) = true

      print(currNode + " ")

      val neighbours = graph(currNode)
      neighbours.foreach { next =>
        if (!visited(next)) {
          queue.enqueue(next)
          visited(next) = true
        }
      }
    }
  }

  private def breadthFirstSearchRec(graph: Graph, startNode: Int): Unit = {
    val queue = MQueue.empty[Int]
    val visited: MSeq[Boolean] = MSeq.fill(graph.size)(false)
    queue.enqueue(startNode)

    def bfsImmutable(queue: Queue[Int], visited: Seq[Boolean]): Unit = {
      if (queue.nonEmpty) {
        val (currNode, queueLeft) = queue.dequeue
        if (visited(currNode)) return

        val newVisited = visited.patch(currNode, Seq(true), 1)

        print(currNode + " ")

        val neighbours = graph(currNode)
        val notVisitedYet = neighbours.foldLeft(queueLeft) { (acc, next) => if (!newVisited(next) && !queue.contains(next)) acc.enqueue(next) else acc }
        bfsImmutable(notVisitedYet, newVisited)

      }
    }

    def bfs(): Unit = {
      if (queue.nonEmpty) {
        val currNode = queue.dequeue()
        if (visited(currNode)) return

        visited(currNode) = true

        print(currNode + " ")

        val neighbours = graph(currNode)
        neighbours.foreach { next =>
          if (!visited(next) && !queue.contains(next)) {
            queue.enqueue(next)
          }
        }
        bfs()
      }
    }

    //    bfsImmutable(Queue(startNode), visited)
    bfs()
  }

  def breadthFirstSearchNoQueue(graph: Graph, startNode: Int): Unit = {

    def bfs(queue: IndexedSeq[Int], queueIdx: Int = 0): Unit = {
      if (queueIdx < queue.size) {

        val currNode = queue(queueIdx)

        print(currNode + " ")

        val neighbours = graph(currNode)
        val newQueue = neighbours.foldLeft(queue) { (acc, next) => if (!queue.contains(next)) acc :+ next else acc }

        bfs(newQueue, queueIdx + 1)
      }
    }

    bfs(IndexedSeq(startNode))
  }

  def shortestPath(graph: Graph, startNode: Int, endNode: Int) = {
    val queue = MQueue.empty[Int]
    val visited: MSeq[Boolean] = MSeq.fill(graph.size)(false)
    val previousNodes: MSeq[Option[Int]] = MSeq.fill(graph.size)(None)
    queue.enqueue(startNode)

    def bfs(): Unit = {
      if (queue.nonEmpty) {
        val currNode = queue.dequeue()
        if (visited(currNode)) return

        visited(currNode) = true

        val neighbours = graph(currNode)
        neighbours.foreach { next =>
          if (!visited(next) && !queue.contains(next)) {
            queue.enqueue(next)
            if (previousNodes(next).isEmpty)
              previousNodes(next) = Some(currNode)
          }
        }
        bfs()
      }
    }

    def reconstructedPath(currNode: Int): Seq[Int] = previousNodes(currNode) match {
      case None => currNode :: Nil
      case Some(differentNode) => currNode +: reconstructedPath(differentNode)
    }

    bfs()
    reconstructedPath(endNode).reverse
  }

}
