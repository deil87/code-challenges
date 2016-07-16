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
    Random.nextInt(100000000) + 1
  }



  val tieuActor = system.actorOf(TieuActor.props, "tieuActor")

//  customersMeta.foreach(cm => system.actorOf(CustomerActor.props(cm.id, cm.orderEstimate), "customerActor"+ cm.id))

  val customersMetaPredef1 = List(CustomersMeta(0, 3), CustomersMeta(1, 9), CustomersMeta(2, 5))

  var totalMinWaiting = 0L
  var totalMinWaitingMetas = 0L
  var totalTimeWaiting = 0L
  var totalBest = 0L
  var total2 = 0L
  var total3 = 0L
//  println("Times: " + customersMeta.map(cm => cm.id + ":" + cm.arrivalTime.toString).mkString(", "))
  @tailrec
  private def calculateMinSumOfWaitingTime(totalServedTime: Long,
                                           currentCustomer: CustomersMeta,
                                           alreadyAwaitingCustomersOrderedByOrderEstimate: List[CustomersMeta],
                                           alreadyAwaitingCustomersOrderedByOrderAwaiting: List[CustomersMeta],
                                           restCustomers: List[CustomersMeta],
                                           accumulatorOfAllWaitingTimes: Long = 0L): Long = {

  val startTimeWaiting = System.nanoTime()
    val withCurrentTotalServedTime = currentCustomer.orderEstimate.toLong + totalServedTime
    val currentWaitingTime =
      if (currentCustomer.arrivalTime.toLong > totalServedTime) currentCustomer.orderEstimate.toLong + currentCustomer.waitingTime // ??currentCustomer.waitingTime
      else withCurrentTotalServedTime - currentCustomer.arrivalTime // ?? + currentCustomer.waitingTime
  val finishTimeWaiting = System.nanoTime()
  totalTimeWaiting = totalTimeWaiting + (finishTimeWaiting - startTimeWaiting)

  if(alreadyAwaitingCustomersOrderedByOrderEstimate.isEmpty && restCustomers.isEmpty)
        accumulatorOfAllWaitingTimes + currentWaitingTime
  else {

        val startingAwaitCustomers = restCustomers.takeWhile(_.arrivalTime <= currentCustomer.arrivalTime + currentCustomer.orderEstimate)
        val  futureCustomers = restCustomers.drop(startingAwaitCustomers.length)

        val startTimeBest = System.nanoTime()

        val startingAwaitOrdByOrderEstimate = startingAwaitCustomers.sortBy(_.orderEstimate)
        val startingAwaitOrdByOrderAwaiting = startingAwaitCustomers.sortBy(_.waitingTime)

    val (bestNext, where) =
      if (alreadyAwaitingCustomersOrderedByOrderEstimate.isEmpty && startingAwaitOrdByOrderEstimate.isEmpty)
        (futureCustomers.head, 2) //
      else {
        val startTime = System.nanoTime()
        val withMinOrderEstimateImpactToOthers = getBestFrom(alreadyAwaitingCustomersOrderedByOrderEstimate, startingAwaitOrdByOrderEstimate, _.orderEstimate < _.orderEstimate)

        val withCurrentMaxWaitingTime = getBestFrom(alreadyAwaitingCustomersOrderedByOrderAwaiting, startingAwaitOrdByOrderAwaiting, _.waitingTime > _.waitingTime)

        val finishTime = System.nanoTime()
        totalMinWaiting = totalMinWaiting + (finishTime - startTime)

        val amountOfWaitingCustomers = alreadyAwaitingCustomersOrderedByOrderEstimate.length + startingAwaitOrdByOrderEstimate.length  - 1
        if ((withMinOrderEstimateImpactToOthers._1.waitingTime + withMinOrderEstimateImpactToOthers._1.orderEstimate * amountOfWaitingCustomers) < withCurrentMaxWaitingTime._1.waitingTime + withCurrentMaxWaitingTime._1.orderEstimate * amountOfWaitingCustomers) {
          withMinOrderEstimateImpactToOthers
        }
        else {
          withCurrentMaxWaitingTime
        }
      }
        val finishTimeBest = System.nanoTime()
        totalBest = totalBest + (finishTimeBest - startTimeBest)

    val waitingDelta = bestNext.orderEstimate + currentCustomer.orderEstimate + currentCustomer.arrivalTime

    calculateMinSumOfWaitingTime(totalServedTime = withCurrentTotalServedTime,
          currentCustomer = bestNext,
          alreadyAwaitingCustomersOrderedByOrderEstimate = {
            val startTimeBest = System.nanoTime()

            val tmp2 =
              if(where == 0) alreadyAwaitingCustomersOrderedByOrderEstimate.diff(List(bestNext)) ::: startingAwaitOrdByOrderEstimate
              else alreadyAwaitingCustomersOrderedByOrderEstimate ::: startingAwaitOrdByOrderEstimate.diff(List(bestNext))
            val finishTimeBest = System.nanoTime()
            total2 = total2 + (finishTimeBest - startTimeBest)
            tmp2.map(c => c.copy(waitingTime = c.waitingTime + waitingDelta - c.arrivalTime ))
          },
          alreadyAwaitingCustomersOrderedByOrderAwaiting = {
            val startTimeBest = System.nanoTime()

            val tmp3 =
              if(where == 0) alreadyAwaitingCustomersOrderedByOrderAwaiting.diff(List(bestNext)) ::: startingAwaitOrdByOrderAwaiting
              else alreadyAwaitingCustomersOrderedByOrderAwaiting ::: startingAwaitOrdByOrderAwaiting.diff(List(bestNext))
            val finishTimeBest = System.nanoTime()
            total3 = total3 + (finishTimeBest - startTimeBest)
            tmp3.map(c => c.copy(waitingTime = c.waitingTime + waitingDelta - c.arrivalTime ))
          },
          restCustomers = futureCustomers,//.diff(List(bestNext)), //Rare case when we are finishing - futureCustomers.head
          accumulatorOfAllWaitingTimes = accumulatorOfAllWaitingTimes + currentWaitingTime)
    }

}

  private def getBestFrom(alreadyAwaiting : List[CustomersMeta], startingAwait : List[CustomersMeta], fun: (CustomersMeta,CustomersMeta) => Boolean): (CustomersMeta, Int) = {
    val minFromAlreadySortedByEstimate = alreadyAwaiting.headOption
    val minFromStartingAwaitSortedByEstimate = startingAwait.headOption

      if (minFromAlreadySortedByEstimate.isDefined && minFromStartingAwaitSortedByEstimate.isDefined)
        if (fun(minFromAlreadySortedByEstimate.get,minFromStartingAwaitSortedByEstimate.get))
          (minFromAlreadySortedByEstimate.get, 0)
        else (minFromStartingAwaitSortedByEstimate.get, 1)
      else if (minFromAlreadySortedByEstimate.isEmpty && minFromStartingAwaitSortedByEstimate.isDefined)
        (minFromStartingAwaitSortedByEstimate.get, 1)
      else if (minFromAlreadySortedByEstimate.isDefined && minFromStartingAwaitSortedByEstimate.isEmpty)
        (minFromAlreadySortedByEstimate.get, 0)
      else throw new IllegalStateException("How do we get here?")
  }

  private def merge(xs : List[CustomersMeta], ys : List[CustomersMeta], fun: (CustomersMeta,CustomersMeta) => Boolean) : List[CustomersMeta] = {
    (xs, ys) match{
      case (Nil, Nil) => Nil
      case (Nil, ys) => ys
      case (xs, Nil) => xs
      case (x :: xs1, y :: ys1) =>
        if(fun(x,y)) x :: merge(xs1, ys, fun)
        else y :: merge(xs, ys1, fun)
    }
  }


  private val firstCustomer: CustomersMeta = customersMeta.head
  val startTime = System.nanoTime()
  val minSum = calculateMinSumOfWaitingTime(0L, firstCustomer, Nil, Nil, customersMeta.tail)
  val finishTime = System.nanoTime()

  println(s"Calculation of average took ${(finishTime - startTime).toDouble / 1000000000} ")
  println(s"Calculation of totalMinWaiting took ${totalMinWaiting.toDouble / 1000000000} ")
  println(s"Calculation of totalMinWaitingMetas took ${totalMinWaitingMetas.toDouble / 1000000000} ")
  println(s"Calculation of totalTimeWaiting took ${totalTimeWaiting.toDouble / 1000000000} ")
  println(s"Calculation of totalBest took ${totalBest.toDouble / 1000000000} ")
  println(s"Calculation of total2 took ${total2.toDouble / 1000000000} ")
  println(s"Calculation of total3 took ${total3.toDouble / 1000000000} ")

  println(s"Min sum of waiting time is: $minSum")

  println(s"Min avg of waiting time is: ${minSum / customersMeta.length}")

  val minSumPredef1 = calculateMinSumOfWaitingTime(0L, customersMetaPredef1.head, Nil, Nil, customersMetaPredef1.tail)

  println(s"==================================\nMin sum of waiting time in customersMetaPredef1 is: $minSumPredef1")

  println(s"Min avg of waiting time in customersMetaPredef1 is: ${minSumPredef1 / customersMetaPredef1.length}")
  system.shutdown()
}

case class CustomersMeta(arrivalTime: Int, orderEstimate: Int, waitingTime: Int = 0)