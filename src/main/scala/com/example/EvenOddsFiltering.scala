package com.example

import scala.collection.mutable
import scala.util.Random

/**
  * Created by deil on 19/09/16.
  */
object EvenOddsFiltering extends App{

  val numbers = Stream.continually(Random.nextInt(10) + 1).take(20000000).toList
  def filterNumbers(numbers: List[Int]) = {
    val tracker = mutable.Map[Int, Int]()
    numbers.foreach { n =>
      tracker.put(n , tracker.get(n).map(_ + 1).getOrElse(1))
    }
    tracker.filter{ case (k, v) => v % 2 == 1}.keys

  }

  def filterNumbers2(numbers: List[Int]) = {
    numbers.groupBy(n => n).filter{case (k, v) => v.size % 2 == 1}.keys
  }

  def filterNumbers3(numbers: List[Int]) = {
    numbers.foldLeft(List[Int]()){ case (acc, next) => if(acc.contains(next)) acc.filter(_ != next) else next :: acc}
  }

  val start = System.currentTimeMillis()
  val res = filterNumbers(numbers)

//  println(numbers.mkString(","))
  println(res.mkString(","))
  val finish = System.currentTimeMillis()
  println(s"It takes ${finish - start} ")

  val start2 = System.currentTimeMillis()
  val res2 = filterNumbers3(numbers)

//  println(numbers.mkString(","))
  println(res2.mkString(","))
  val finish2 = System.currentTimeMillis()
  println(s"It takes with 2 method ${finish2 - start2} ")

}
