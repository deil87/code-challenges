package com.example

import akka.actor.ActorSystem

import scala.annotation.tailrec
import scala.util.Random

object PizzaRestaurant extends App {
  val system = ActorSystem("PizzaRestaurantSystem")

  val numberOfCustomers = 100000 // Really popular restaurant!

  val customersMeta = (0 until numberOfCustomers)
    .map(i => CustomersMeta( randomArrivalTime, orderEstimate = randomPizzaTimeCost))
    .sortBy(_.arrivalTime).toList

  // Note: randomPizzaTimeCost and randomArrivalTime should be of similar order. If arrival time >> pizza cooking time, Tieu will be bored (and beggar).
  def randomArrivalTime: Int = {
    Random.nextInt(1000000001)
  }

  def randomPizzaTimeCost: Int = {
    Random.nextInt(2000000) + 1
  }



  val tieuActor = system.actorOf(TieuActor.props, "tieuActor")

//  customersMeta.foreach(cm => system.actorOf(CustomerActor.props(cm.id, cm.orderEstimate), "customerActor"+ cm.id))

  val customersMetaPredef1 = List(CustomersMeta(0, 3), CustomersMeta(1, 9), CustomersMeta(2, 5))

  var totalMinWaiting = 0L
  var totalMinWaitingMetas = 0L
  var totalTimeWaiting = 0L
  var totalBest = 0L
//  println("Times: " + customersMeta.map(cm => cm.id + ":" + cm.arrivalTime.toString).mkString(", "))
  @tailrec
  private def calculateMinSumOfWaitingTime(totalServedTime: Long, currentCustomer: CustomersMeta, alreadyAwaitingCustomers: List[CustomersMeta], restCustomers: List[CustomersMeta], accumulatorOfAllWaitingTimes: Long = 0L): Long = {
//    println(s"Serving $currentCustomer ...")
  val startTimeWaiting = System.nanoTime()
    val withCurrentTotalServedTime = currentCustomer.orderEstimate.toLong + totalServedTime
    val currentWaitingTime =
      if (currentCustomer.arrivalTime.toLong > totalServedTime) currentCustomer.orderEstimate.toLong + currentCustomer.waitingTime // ??currentCustomer.waitingTime
      else withCurrentTotalServedTime - currentCustomer.arrivalTime // ?? + currentCustomer.waitingTime
  val finishTimeWaiting = System.nanoTime()
  totalTimeWaiting = totalTimeWaiting + (finishTimeWaiting - startTimeWaiting)

  if(alreadyAwaitingCustomers.isEmpty && restCustomers.isEmpty)
        accumulatorOfAllWaitingTimes + currentWaitingTime
  else {

        // next tree rows 1 sec of 87
        val startingAwaitCustomers = restCustomers.takeWhile(_.arrivalTime <= currentCustomer.arrivalTime + currentCustomer.orderEstimate)
        val customersAwaitingServing = alreadyAwaitingCustomers ::: startingAwaitCustomers
        val  futureCustomers = restCustomers.drop(startingAwaitCustomers.length)

        val startTimeBest = System.nanoTime()

        val bestNext = customersAwaitingServing match {
          case Nil => futureCustomers.head
          case ca =>
            val startTime = System.nanoTime()
            val withMinOrderEstimateImpactToOthers = ca.minBy(_.orderEstimate)
            val withCurrentMaxWaitingTime = ca.maxBy(_.waitingTime)

            val finishTime = System.nanoTime()
            totalMinWaiting = totalMinWaiting + (finishTime - startTime)

            val amountOfWaitingCustomers = ca.length - 1
            if ((withMinOrderEstimateImpactToOthers.waitingTime + withMinOrderEstimateImpactToOthers.orderEstimate * amountOfWaitingCustomers) < withCurrentMaxWaitingTime.waitingTime + withCurrentMaxWaitingTime.orderEstimate * amountOfWaitingCustomers) {
              withMinOrderEstimateImpactToOthers
            }
            else {
              withCurrentMaxWaitingTime
            }
        }
        val finishTimeBest = System.nanoTime()
        totalBest = totalBest + (finishTimeBest - startTimeBest)

        //        println(s"Waiting time of $currentCustomer : $currentWaitingTime")
        calculateMinSumOfWaitingTime(withCurrentTotalServedTime, bestNext, customersAwaitingServing.diff(List(bestNext)).map(c => c.copy(waitingTime = c.waitingTime + bestNext.orderEstimate + (currentCustomer.orderEstimate - (c.arrivalTime - currentCustomer.arrivalTime)))), futureCustomers,
          accumulatorOfAllWaitingTimes = accumulatorOfAllWaitingTimes + currentWaitingTime)
    }

}


  private val firstCustomer: CustomersMeta = customersMeta.head
  val startTime = System.nanoTime()
  val minSum = calculateMinSumOfWaitingTime(0L, firstCustomer, Nil, customersMeta.tail)
  val finishTime = System.nanoTime()

  println(s"Calculation of average took ${(finishTime - startTime).toDouble / 1000000000} ")
  println(s"Calculation of totalMinWaiting took ${totalMinWaiting.toDouble / 1000000000} ")
  println(s"Calculation of totalMinWaitingMetas took ${totalMinWaitingMetas.toDouble / 1000000000} ")
  println(s"Calculation of totalTimeWaiting took ${totalTimeWaiting.toDouble / 1000000000} ")
  println(s"Calculation of totalBest took ${totalBest.toDouble / 1000000000} ")

  println(s"Min sum of waiting time is: $minSum")

  println(s"Min avg of waiting time is: ${minSum / customersMeta.length}")

  val minSumPredef1 = calculateMinSumOfWaitingTime(0L, customersMetaPredef1.head,  Nil,customersMetaPredef1.tail)

  println(s"==================================\nMin sum of waiting time in customersMetaPredef1 is: $minSumPredef1")

  println(s"Min avg of waiting time in customersMetaPredef1 is: ${minSumPredef1 / customersMetaPredef1.length}")
  system.shutdown()
}

case class CustomersMeta(arrivalTime: Int, orderEstimate: Int, waitingTime: Int = 0)