package com.rewera.graphtheory

import scala.Double.{NegativeInfinity, PositiveInfinity => P8}

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
    val result = floydWarshall(theGraph)
    println(result.map(_.mkString("[", ", ", "]")).mkString("[", "\n", "]"))
  }

  def floydWarshall(graph: WeightedGraphAdjacencyMatrix): Array[Array[Double]] = {
    val memoTable: Array[Array[Double]] = Array.tabulate(graph.length)(graph)

    memoTable.indices.tail.foreach { k =>
      memoTable.indices.foreach { i =>
        memoTable.indices.foreach { j =>
          memoTable(i)(j) = Math.min(memoTable(i)(j), memoTable(i)(k) + memoTable(k)(j))
        }
      }
    }

    memoTable.indices.tail.foreach { k =>
      memoTable.indices.foreach { i =>
        memoTable.indices.foreach { j =>
          if (memoTable(i)(k) + memoTable(k)(j) < memoTable(i)(j))
            memoTable(i)(j) = NegativeInfinity
        }
      }
    }

    memoTable
  }
}
