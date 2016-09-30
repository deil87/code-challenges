package com.example


import java.util.Scanner

/**
  * Created by deil on 10/09/16.
  * Imperative version
  */
object SelectionSort {
  def main(args: Array[String]) { // what if we have linked list

    val s = new Scanner(System.in)
    val a = s.nextInt()
    val numbers: Array[Int] = Array.fill(a){0}
    for(i <- 0 until a) {
      numbers(i) = s.nextInt()
    }

    println(sort(numbers).mkString(","))
  }

  def sort(numbers: Array[Int]): Array[Int] = {
    val n = numbers.size

    for {i <- 0 until n} yield {
      var minOfLeft = i
      for {j <- i + 1 until n} yield {
        if (numbers(j) < numbers(i)) minOfLeft = j
      }
      val temp = numbers(i)
      numbers(i) = numbers(minOfLeft)
      numbers(minOfLeft) = temp

    }
    numbers
  }

}
