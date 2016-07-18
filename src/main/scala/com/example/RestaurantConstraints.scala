package com.example

import scala.util.Random

/**
  * Created by deil on 18/07/16.
  */
trait RestaurantConstraints {
  val numberOfCustomers = 10000 // Really popular restaurant!

  // Note: randomPizzaTimeCost and randomArrivalTime should be of similar order. If arrival time >> pizza cooking time, Tieu will be bored (and beggar).
  def randomArrivalTime = Random.nextInt(1000000001)

  def randomPizzaTimeCost = Random.nextInt(1000000000) + 1

}
