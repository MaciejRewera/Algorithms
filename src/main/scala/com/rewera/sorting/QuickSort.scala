package com.rewera.sorting

import scala.util.Random

object QuickSort {

  def main(args: Array[String]): Unit = {
    val inputArray = Array(4,7,3,9,6,8,1,5,2)

    println(runQuickSort(inputArray).mkString("[", ", ", "]"))

    val size = 100
    val largeInput = Array.fill(size) { Random.nextInt(size) }
    println(runQuickSort(largeInput).mkString("[", ", ", "]"))
  }

  def runQuickSort(input: Array[Int]): Array[Int] = {

    def runQuickSort(input: Array[Int], startIdx: Int, endIdx: Int): Unit = if (startIdx < endIdx) {
      val partitionIdx = partition(input, startIdx, endIdx)
      runQuickSort(input, startIdx, partitionIdx)
      runQuickSort(input, partitionIdx + 1, endIdx)
    }

    runQuickSort(input, 0, input.length - 1)
    input
  }

  private def partition(input: Array[Int], startIdx: Int, endIdx: Int): Int = {
    val pivot = input((endIdx + startIdx) / 2)

    var i = startIdx - 1
    var j = endIdx + 1

    def swap(idx1: Int, idx2: Int): Unit = {
      val tmp = input(idx1)
      input(idx1) = input(idx2)
      input(idx2) = tmp
    }

    while(true) {
      do {
        j = j - 1
      } while (input(j) > pivot)
      do {
        i = i + 1
      } while (input(i) < pivot)

      if (i < j)
        swap(i, j)
      else
        return j
    }

    j
  }

}
