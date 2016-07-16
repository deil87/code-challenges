package com.example

import akka.actor.ActorSystem

import scala.util.Random

object PizzaRestaurant extends App {
  val system = ActorSystem("PizzaRestaurantSystem")

  val numberOfCustomers = 1000 // Really popular restaurant!
  val maxPermutationsThreshold = 20

  val customersMeta = (0 to numberOfCustomers)
    .map(i => CustomersMeta(i, i, orderEstimate = Random.nextInt(8) + 2))
    .sortBy(_.arrivalTime).toList

  val tieuActor = system.actorOf(TieuActor.props, "tieuActor")

//  customersMeta.foreach(cm => system.actorOf(CustomerActor.props(cm.id, cm.orderEstimate), "customerActor"+ cm.id))

  val customersMetaPredef1 = List(CustomersMeta(1, 0, 3), CustomersMeta(2, 1, 9), CustomersMeta(3, 2, 5))

  println("Times: " + customersMeta.map(cm => cm.id + ":" + cm.arrivalTime.toString).mkString(", "))
  def calculateMinSumOfWaitingTime(served: List[CustomersMeta], currentCustomer: CustomersMeta, restCustomers: List[CustomersMeta]): Int = {
    println(s"Serving $currentCustomer ...")
    val servedWithCurrent = currentCustomer :: served
    restCustomers match {
      case Nil => {
        println(s"Serving last customer $currentCustomer")
        servedWithCurrent.foldLeft(0)((res, next) => res + next.orderEstimate) - currentCustomer.arrivalTime
      }
      case rc => {
        val customersArrivedDuringCurrent = restCustomers.takeWhile(_.arrivalTime <= currentCustomer.arrivalTime + currentCustomer.orderEstimate)
        println(s"Customers arrived during serving pizza for ${currentCustomer.id}: ${customersArrivedDuringCurrent.mkString(",")}")
        val bestNext = customersArrivedDuringCurrent match {
          case Nil => restCustomers.head
          case ca if ca.length > maxPermutationsThreshold =>
            ca.map {
              awaitingCustomer => (awaitingCustomer, currentCustomer.orderEstimate - (awaitingCustomer.arrivalTime - currentCustomer.arrivalTime))
            }.sortBy(_._2).head._1
          case ca =>
            println(s"Using permutations.................................size of source list is ${ca.length}")
            val res = ca.permutations.toList.map {
              awaitingCustomers => (awaitingCustomers.head, awaitingCustomers.foldLeft(currentCustomer.orderEstimate)((res, next) => res + (currentCustomer.orderEstimate - (next.arrivalTime - currentCustomer.arrivalTime)) + next.orderEstimate) / awaitingCustomers.length)
            }.sortBy(_._2)
            res.head._1

        }
        def waitingTimeOfNext =
          calculateMinSumOfWaitingTime(servedWithCurrent, bestNext, restCustomers.diff(List(bestNext)).map(c => c.copy(waitingTime = c.waitingTime + bestNext.orderEstimate + (currentCustomer.orderEstimate - (c.arrivalTime - currentCustomer.arrivalTime)))))
        val currentWaitingTime = servedWithCurrent.foldLeft(0)((res, next) => res + next.orderEstimate) - currentCustomer.arrivalTime
        println(s"Waiting time of $currentCustomer - $currentWaitingTime")
        currentWaitingTime + waitingTimeOfNext
      }
    }
  }


  private val firstCustomer: CustomersMeta = customersMeta.head
  val minSum = calculateMinSumOfWaitingTime(Nil, firstCustomer, customersMeta.tail)

  println(s"Min sum of waiting time is: $minSum")

  println(s"Min avg of waiting time is: ${minSum / customersMeta.length}")

  /*val minSumPredef1 = calculateMinSumOfWaitingTime(Nil, customersMetaPredef1.head, customersMetaPredef1.tail)

  println(s"Min sum of waiting time in customersMetaPredef1 is: $minSumPredef1")

  println(s"Min avg of waiting time in customersMetaPredef1 is: ${minSumPredef1 / customersMetaPredef1.length}")
*/
//  system.awaitTermination()
  system.shutdown()
}

case class CustomersMeta(id: Int, arrivalTime: Int, orderEstimate: Int, waitingTime: Int = 0)