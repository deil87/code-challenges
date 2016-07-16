package com.example

import akka.actor.ActorSystem

import scala.annotation.tailrec
import scala.util.Random

object PizzaRestaurant extends App {
  val system = ActorSystem("PizzaRestaurantSystem")

  val numberOfCustomers = 100000 // Really popular restaurant!

  val customersMeta = (0 until numberOfCustomers)
    .map(i => CustomersMeta(i, randomArrivalTime, orderEstimate = randomPizzaTimeCost))
    .sortBy(_.arrivalTime).toList

  // Note: randomPizzaTimeCost and randomArrivalTime should be of similar order. If arrival time >> pizza cooking time, Tieu will be bored (and beggar).
  def randomPizzaTimeCost: Int = {
    Random.nextInt(100) + 1
  }

  def randomArrivalTime: Int = {
    Random.nextInt(1000000001)
  }

  val tieuActor = system.actorOf(TieuActor.props, "tieuActor")

//  customersMeta.foreach(cm => system.actorOf(CustomerActor.props(cm.id, cm.orderEstimate), "customerActor"+ cm.id))

  val customersMetaPredef1 = List(CustomersMeta(1, 0, 3), CustomersMeta(2, 1, 9), CustomersMeta(3, 2, 6))

  var totalMinWaiting = 0L
//  println("Times: " + customersMeta.map(cm => cm.id + ":" + cm.arrivalTime.toString).mkString(", "))
  @tailrec
  private def calculateMinSumOfWaitingTime(totalServedTime: Long, currentCustomer: CustomersMeta, restCustomers: List[CustomersMeta], accumulatorOfAllWaitingTimes: Long = 0L): Long = {
//    println(s"Serving $currentCustomer ...")
    val withCurrentTotalServedTime = currentCustomer.orderEstimate.toLong + totalServedTime
    val currentWaitingTime =
      if (currentCustomer.arrivalTime.toLong > totalServedTime) currentCustomer.orderEstimate.toLong
      else withCurrentTotalServedTime - currentCustomer.arrivalTime

//    println(accumulatorOfAllWaitingTimes)

  restCustomers match {
      case Nil =>
//        println(s"Serving last customer $currentCustomer")
        accumulatorOfAllWaitingTimes + currentWaitingTime
      case rc =>
        val startTime = System.nanoTime()
//        val (customersArrivedDuringCurrent, futureCustomers) = restCustomers.partition(_.arrivalTime <= currentCustomer.arrivalTime + currentCustomer.orderEstimate)
        val customersArrivedDuringCurrent= restCustomers.takeWhile(_.arrivalTime <= currentCustomer.arrivalTime + currentCustomer.orderEstimate)
        val  futureCustomers = restCustomers.drop(customersArrivedDuringCurrent.length)

        val finishTime = System.nanoTime()
        totalMinWaiting = totalMinWaiting + (finishTime - startTime)
//        println(s"Customers arrived during serving pizza for ${currentCustomer.id}: ${customersArrivedDuringCurrent.mkString(",")}")
        val bestNext = customersArrivedDuringCurrent match {
          case Nil => restCustomers.head
          case ca =>
            val withMinOrderEstimateImpactToOthers = ca.minBy(_.orderEstimate)
            val withCurrentMaxWaitingTime = ca.maxBy(_.waitingTime)

            val amountOfWaitingCustomers = ca.length - 1
            if ((withMinOrderEstimateImpactToOthers.waitingTime + withMinOrderEstimateImpactToOthers.orderEstimate * amountOfWaitingCustomers) < withCurrentMaxWaitingTime.waitingTime + withCurrentMaxWaitingTime.orderEstimate * amountOfWaitingCustomers) {
//              println("withMinOrderEstimateImpactToOthers")
              withMinOrderEstimateImpactToOthers
            }
            else {
//              println("withCurrentMaxWaitingTime")
              withCurrentMaxWaitingTime
            }
        }
//        println(s"Waiting time of $currentCustomer : $currentWaitingTime")
        calculateMinSumOfWaitingTime(withCurrentTotalServedTime, bestNext, customersArrivedDuringCurrent.diff(List(bestNext)).map(c => c.copy(waitingTime = c.waitingTime + bestNext.orderEstimate + (currentCustomer.orderEstimate - (c.arrivalTime - currentCustomer.arrivalTime)))) ::: futureCustomers,
          accumulatorOfAllWaitingTimes = accumulatorOfAllWaitingTimes + currentWaitingTime)
    }
  }


  private val firstCustomer: CustomersMeta = customersMeta.head
  val startTime = System.nanoTime()
  val minSum = calculateMinSumOfWaitingTime(0L, firstCustomer, customersMeta.tail)
  val finishTime = System.nanoTime()

  println(s"Calculation of average took ${(finishTime - startTime).toDouble / 1000000000} ")
  println(s"Calculation of totalMinWaiting took ${totalMinWaiting.toDouble / 1000000000} ")

  println(s"Min sum of waiting time is: $minSum")

  println(s"Min avg of waiting time is: ${minSum / customersMeta.length}")

  /*val minSumPredef1 = calculateMinSumOfWaitingTime(Nil, customersMetaPredef1.head, customersMetaPredef1.tail)

  println(s"Min sum of waiting time in customersMetaPredef1 is: $minSumPredef1")

  println(s"Min avg of waiting time in customersMetaPredef1 is: ${minSumPredef1 / customersMetaPredef1.length}")*/
  system.shutdown()
}

case class CustomersMeta(id: Int, arrivalTime: Int, orderEstimate: Int, waitingTime: Int = 0)