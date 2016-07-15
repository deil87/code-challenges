package com.example

import akka.actor.ActorSystem

import scala.util.Random

object PizzaRestaurant extends App {
  val system = ActorSystem("PizzaRestaurantSystem")

  val numberOfCustomers = 5

  val customersMeta = (1 to numberOfCustomers)
    .map(i => CustomersMeta(i, Random.nextInt(100), orderEstimate = Random.nextInt(8) + 2))
    .sortBy(_.arrivalTime).toList

  val tieuActor = system.actorOf(TieuActor.props, "tieuActor")

//  customersMeta.foreach(cm => system.actorOf(CustomerActor.props(cm.id, cm.orderEstimate), "customerActor"+ cm.id))

  val customersMetaPredef1 = List(CustomersMeta(1, 0, 3), CustomersMeta(2, 1, 9), CustomersMeta(3, 2, 6))

  //Take the first one

  def calculateMinSumOfWaitingTime(currentCustomer: CustomersMeta, restCustomers: List[CustomersMeta]): Int = restCustomers match {
    case Nil => currentCustomer.orderEstimate
    case rc => {
      val customersArrivedDuringCurrent = restCustomers.takeWhile(_.arrivalTime <= currentCustomer.arrivalTime + currentCustomer.orderEstimate)
      val bestNext = customersArrivedDuringCurrent match {
        case Nil => restCustomers.head
        case ca => ca.permutations.toList.map {
          futureCustomers => (futureCustomers.head, futureCustomers.foldLeft(currentCustomer.orderEstimate)((res, next) => res + (currentCustomer.orderEstimate - (next.arrivalTime - currentCustomer.arrivalTime)) + next.orderEstimate) / futureCustomers.length)
        }.sortBy(_._2).head._1
      }
      def waitingTimeOfNext =
        calculateMinSumOfWaitingTime(bestNext, restCustomers.diff(List(bestNext)).map(c => c.copy(orderEstimate = c.orderEstimate + bestNext.orderEstimate)))
      currentCustomer.orderEstimate + waitingTimeOfNext // Maybe diff is not that efficient; Is this sorted?
    }
  }



  /*val minSum = calculateMinSumOfWaitingTime(customersMeta.head, customersMeta.tail)

  println(s"Min sum of waiting time is: $minSum")

  println(s"Min avg of waiting time is: ${minSum / customersMeta.length}")*/

  val minSumPredef1 = calculateMinSumOfWaitingTime(customersMetaPredef1.head, customersMetaPredef1.tail)

  println(s"Min sum of waiting time in customersMetaPredef1 is: $minSumPredef1")

  println(s"Min avg of waiting time in customersMetaPredef1 is: ${minSumPredef1 / customersMetaPredef1.length}")

  system.awaitTermination()
}

case class CustomersMeta(id: Int, arrivalTime: Int, orderEstimate: Int)