package com.rewera.statistics

import scala.util.Random

object PositionalStatistics {

  def main(args: Array[String]): Unit = {

    val inputSize = 10000000
    val input = Seq.tabulate(inputSize)(_ => Random.nextInt(inputSize + 1))
    val indexedInput = input.toIndexedSeq

//    println(input)
    println(min(input))
    println(max(input))
    println(minMax(input))
    println(minMaxProposition(indexedInput))
  }

  def min(input: Seq[Int]): Int =
    input.tail.foldLeft(input.head) { (min, elem) =>
      if (elem < min) elem
      else min
    }

  def max(input: Seq[Int]): Int =
    input.tail.foldLeft(input.head) { (max, elem) =>
      if (elem > max) elem
      else max
    }

  def minMax(input: Seq[Int]): (Int, Int) = (min(input), max(input))

  def minMaxProposition(input: IndexedSeq[Int]): (Int, Int) = {
    var min = Math.min(input(0), input(1))
    var max = Math.max(input(0), input(1))
    var i = 2

    while(i < input.length) {
      val first = input(i)
      val second = input(i + 1)

      if (first > second) {
        if (first > max)
          max = first
        else if (second < min)
          min = second

      } else {
        if (second > max)
          max = second
        else if (first < min)
          min = first
      }

      i = i + 2
    }

    (min, max)
  }

}
