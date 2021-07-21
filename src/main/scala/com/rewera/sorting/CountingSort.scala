package com.rewera.sorting

import scala.util.Random

object CountingSort {

  def main(args: Array[String]): Unit = {
    val inputArray = Array(4,7,3,9,6,8,1,5,2)

    println(countingSort(inputArray, 10).mkString("[", ", ", "]"))
    println(countingSortForwardCounterIteration(inputArray).mkString("[", ", ", "]"))

    val size = 100
    val largeInput = Array.fill(size) { Random.nextInt(size) }
    println(countingSort(largeInput, size).mkString("[", ", ", "]"))
    println(countingSortForwardCounterIteration(largeInput).mkString("[", ", ", "]"))

  }

  def countingSort(input: Array[Int], k: Int): Array[Int] = {
    val counters: Array[Int] = Array.fill(k)(0)

    input.foreach(elem => counters(elem) = counters(elem) + 1)

    (1 until counters.length).foreach(i => counters(i) = counters(i) + counters(i - 1))

    val result = Array.fill(input.length)(0)

    input.foreach { elem =>
      result(counters(elem) - 1) = elem
      counters(elem) = counters(elem) - 1
    }

    result
  }

  def countingSortForwardCounterIteration(input: Array[Int]): Seq[Int] = {
    val countersSizeConsideringZeros = input.max + 1
    val counters: Array[Int] = Array.fill(countersSizeConsideringZeros)(0)

    input.foreach(elem => counters(elem) = counters(elem) + 1)

    counters.zipWithIndex.foldLeft(Seq.empty[Int]) {
      case (acc, (0, _)) => acc
      case (acc, (amount, idx)) => Seq.fill(amount)(idx) ++ acc
    }.reverse
  }

}
