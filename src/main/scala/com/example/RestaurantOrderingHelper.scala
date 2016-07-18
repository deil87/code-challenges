package com.example

import scala.util.Random

/**
  * Created by deil on 18/07/16.
  */
trait RestaurantOrderingHelper {

  val fragmentsOrdering = new Ordering[List[(Int, CustomerMeta)]] {
    override def compare(xs: List[(Int, CustomerMeta)], ys: List[(Int, CustomerMeta)]): Int = {
      (xs, ys) match {
        case (Nil, Nil) => 0
        case (Nil, ys) => 1 // reversed  if one list is empty then another one is returned
        case (xs, Nil) => -1
        case (xs, ys) => Ordering[Int].compare(xs.head._1, ys.head._1)
      }
    }
  }
}
