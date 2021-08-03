package com.rewera.graphtheory

import scala.Double.{NegativeInfinity, PositiveInfinity => P8}
import scala.collection.mutable.ListBuffer

object FloydWarshall {

  private type WeightedGraphAdjacencyMatrix = Array[Array[Double]]

  private val theGraph: WeightedGraphAdjacencyMatrix = Array(
    Array(P8, 3, 6, P8, P8, P8, P8, P8),
    Array(P8, P8, 4, 4, 11, P8, P8, P8),
    Array(P8, P8, P8, 8, P8, P8, 11, P8),
    Array(P8, P8, P8, P8, -4, 5, 2, P8),
    Array(P8, P8, P8, P8, P8, P8, P8, 9),
    Array(P8, P8, P8, P8, P8, P8, P8, 1),
    Array(P8, P8, P8, P8, P8, P8, P8, 2),
    Array(P8, P8, P8, P8, P8, P8, P8, P8)
  )

  private val theGraphWithNegativeCycle: WeightedGraphAdjacencyMatrix = Array(

  )

  def main(args: Array[String]): Unit = {
    val (memoTable, nextTable) = floydWarshall(theGraph)

    printTable(nextTable)
    println
    printAllPaths(memoTable, nextTable)
  }

  def floydWarshall(graph: WeightedGraphAdjacencyMatrix): (Array[Array[Double]], Array[Array[Int]]) = {
    val memoTable: Array[Array[Double]] = Array.tabulate(graph.length)(graph)
    val nextTable = initNext(graph)

    memoTable.indices.tail.foreach { k =>
      memoTable.indices.foreach { i =>
        memoTable.indices.foreach { j =>

          val newPathWeight = memoTable(i)(k) + memoTable(k)(j)
          if (newPathWeight < memoTable(i)(j)) {
            memoTable(i)(j) = newPathWeight
            nextTable(i)(j) = nextTable(i)(k)
          }

        }
      }
    }

    memoTable.indices.tail.foreach { k =>
      memoTable.indices.foreach { i =>
        memoTable.indices.foreach { j =>
          if (memoTable(i)(k) + memoTable(k)(j) < memoTable(i)(j)) {
            memoTable(i)(j) = NegativeInfinity
            nextTable(i)(j) = -1
          }
        }
      }
    }

    (memoTable, nextTable)
  }

  private def initNext(graph: WeightedGraphAdjacencyMatrix): Array[Array[Int]] = {
    val nextTable = Array.fill(graph.length)(Array.fill(graph.length)(-1))

    graph.indices.foreach { i =>
      graph.indices.foreach { j =>
        if (graph(i)(j) != P8)
          nextTable(i)(j) = j
      }
    }

    nextTable
  }

  private def printTable[T](table: Array[Array[T]]): Unit = println(table.map(_.mkString("[", ", ", "]")).mkString("", "\n", ""))

  private def printAllPaths(memoTable: Array[Array[Double]], nextTable: Array[Array[Int]]): Unit = {
    memoTable.indices.foreach { i =>
      memoTable.indices.foreach { j =>
        val currPath = reconstructPath(memoTable, nextTable, i, j)
        if (currPath.isEmpty)
          println(s"Path from $i to $j does not exist")
        else
          println(s"Path from $i to $j: ${currPath.mkString(" -> ")}")

      }
    }
  }

  def reconstructPath(memoTable: Array[Array[Double]], nextTable: Array[Array[Int]], startNode: Int, endNode: Int): List[Int] = {
    val path = ListBuffer[Int]()

    if (memoTable(startNode)(endNode) == P8) return List[Int]()

    var at = startNode
    while (at != endNode) {
      if (at == -1) return List[Int]()

      path.addOne(at)
      at = nextTable(at)(endNode)
    }

    path.addOne(endNode)
    path.toList
  }


}
