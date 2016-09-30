package com.example

import java.util.Scanner

import scala.annotation.tailrec

/**
  * Created by deil on 10/09/16.
  */
object InsertionSort {
  def main(args: List[String]) {

    val s = new Scanner(System.in)
    val a = s.nextInt()
    val numbers: Array[Int] = Array.fill(a){0}
    for(i <- 0 until a) {
      numbers(i) = s.nextInt()
    }

    println(sort(numbers.toList).mkString(","))
  }

  def insert(element: Int, acc: List[Int]): List[Int] = {
    def insertHelper(elements: List[Int]): List[Int] = elements match {
      case Nil => List(element)
      case head :: tail if element < head  => element :: head :: tail
      case head :: tail => head :: insert(element, tail)
    }

    insertHelper(acc)
  }


  def sort(numbers: List[Int]): List[Int] = {
    def sortHelper(unsorted: List[Int], acc: List[Int]): List[Int] = unsorted match {
      case Nil => acc
      case head :: tail => sortHelper(tail, insert(head, acc))
    }
    sortHelper(numbers, Nil)
  }

}
