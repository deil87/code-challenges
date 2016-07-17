package com.example

import java.util

import akka.actor.ActorSystem

import scala.annotation.tailrec
import scala.util.{Failure, Random, Success, Try}

object PizzaRestaurant extends App {
  val system = ActorSystem("PizzaRestaurantSystem")

  val numberOfCustomers = 100000 // Really popular restaurant!

  val customersMeta = (0 until numberOfCustomers)
    .map(i => CustomersMeta(i, randomArrivalTime, orderEstimate = randomPizzaTimeCost))
    .sortBy(_.arrivalTime).toList

  // Note: randomPizzaTimeCost and randomArrivalTime should be of similar order. If arrival time >> pizza cooking time, Tieu will be bored (and beggar).
  def randomArrivalTime: Int = {
    Random.nextInt(100000001)
  }

  def randomPizzaTimeCost: Int = {
    Random.nextInt(100000000) + 1
  }

  val tieuActor = system.actorOf(TieuActor.props, "tieuActor")

  val customersMetaPredef1 = List(CustomersMeta(0,0, 3), CustomersMeta(1,1, 9), CustomersMeta(2,2, 5))

  var totalMinWaiting = 0L
  var totalMinWaitingMetas = 0L
  var totalTimeWaiting = 0L
  var totalBest = 0L
  var total2 = 0L
  var total3  = 0L
  var totalRemove2  = 0L

  @tailrec
  private def calculateMinSumOfWaitingTime(totalServedTime: Long,
                                           currentCustomer: CustomersMeta,
                                           alreadyAwaitingCustomersOrderedByOrderEstimate: Array[List[(Int, CustomersMeta)]],
                                           alreadyAwaitingCustomersOrderedByOrderAwaiting: Array[List[(Int, CustomersMeta)]],
                                           restCustomers: List[CustomersMeta],
                                           accumulatorOfAllWaitingTimes: Long = 0L): Long = {

  val startTimeWaiting = System.nanoTime()
    val withCurrentTotalServedTime = currentCustomer.orderEstimate.toLong + totalServedTime
    val currentWaitingTime =
      if (currentCustomer.arrivalTime.toLong > totalServedTime) currentCustomer.orderEstimate.toLong + currentCustomer.waitingTime // ??currentCustomer.waitingTime
      else withCurrentTotalServedTime - currentCustomer.arrivalTime // ?? + currentCustomer.waitingTime
  val finishTimeWaiting = System.nanoTime()
  totalTimeWaiting = totalTimeWaiting + (finishTimeWaiting - startTimeWaiting)

  if(restCustomers.isEmpty && alreadyAwaitingCustomersOrderedByOrderEstimate.forall(_.isEmpty))
    accumulatorOfAllWaitingTimes + currentWaitingTime
  else {
    val startingAwaitCustomers = restCustomers.takeWhile(_.arrivalTime <= currentCustomer.arrivalTime + currentCustomer.orderEstimate)

    val  futureCustomers = restCustomers.drop(startingAwaitCustomers.length)

    val startingAwaitOrdByOrderEstimate = startingAwaitCustomers.sortBy(_.orderEstimate).map(cm => (cm.orderEstimate, cm))
    val startingAwaitOrdByOrderAwaiting = startingAwaitCustomers.map(cm => (cm.waitingTime, cm))

    val (bestNext, indexOfFragment) =
      if (alreadyAwaitingCustomersOrderedByOrderEstimate.isEmpty && startingAwaitOrdByOrderEstimate.isEmpty)
        (futureCustomers.head, -3) // case when we just getting next
      else {
        val withMinOrderEstimateImpactToOthers = getBestFrom(alreadyAwaitingCustomersOrderedByOrderEstimate, startingAwaitOrdByOrderEstimate, _.orderEstimate < _.orderEstimate)

        val withCurrentMaxWaitingTime = getBestFrom(alreadyAwaitingCustomersOrderedByOrderAwaiting, startingAwaitOrdByOrderAwaiting, _.waitingTime > _.waitingTime)

        val amountOfWaitingCustomers = alreadyAwaitingCustomersOrderedByOrderEstimate.map(_.size).sum + startingAwaitOrdByOrderEstimate.length - 1

        if ((withMinOrderEstimateImpactToOthers._1.waitingTime + withMinOrderEstimateImpactToOthers._1.orderEstimate * amountOfWaitingCustomers) < withCurrentMaxWaitingTime._1.waitingTime + withCurrentMaxWaitingTime._1.orderEstimate * amountOfWaitingCustomers) {
          withMinOrderEstimateImpactToOthers
        }
        else {
          withCurrentMaxWaitingTime
        }
      }

    val waitingDelta = bestNext.orderEstimate + currentCustomer.orderEstimate + currentCustomer.arrivalTime

    calculateMinSumOfWaitingTime(totalServedTime = withCurrentTotalServedTime,
          currentCustomer = bestNext,
          alreadyAwaitingCustomersOrderedByOrderEstimate = {

            alreadyAwaitingCustomersOrderedByOrderEstimate.foreach{fs => fs.foreach{ case (v, cm) =>
              cm.waitingTime = cm.waitingTime + bestNext.orderEstimate
            }}
            startingAwaitOrdByOrderEstimate.foreach{case (v, cm) =>
              val delta = waitingDelta - cm.arrivalTime
              cm.waitingTime = cm.waitingTime + delta
            }

            val tmp2 =
              if(indexOfFragment >= 0) {
                alreadyAwaitingCustomersOrderedByOrderEstimate.update(indexOfFragment, alreadyAwaitingCustomersOrderedByOrderEstimate(indexOfFragment).tail)
                alreadyAwaitingCustomersOrderedByOrderEstimate ++ List(startingAwaitOrdByOrderEstimate)
              }
              else if(indexOfFragment == -2) {
                val from2 = removeElemFrom(startingAwaitOrdByOrderEstimate, bestNext)
                alreadyAwaitingCustomersOrderedByOrderEstimate ++ List(from2)
              } else {
                //for -3 case when no awaiting customers at all
                alreadyAwaitingCustomersOrderedByOrderAwaiting
              }
            tmp2
          },
          alreadyAwaitingCustomersOrderedByOrderAwaiting = {

            alreadyAwaitingCustomersOrderedByOrderAwaiting.foreach{fs => fs.foreach{ case (v, cm) =>
              cm.waitingTime = cm.waitingTime + bestNext.orderEstimate
            }}
            startingAwaitOrdByOrderAwaiting.foreach{case (v, cm) =>
              val delta = waitingDelta - cm.arrivalTime
              cm.waitingTime = cm.waitingTime + delta
            }
            val tmp3 =
              if(indexOfFragment >= 0) {
                alreadyAwaitingCustomersOrderedByOrderAwaiting.update(indexOfFragment, alreadyAwaitingCustomersOrderedByOrderAwaiting(indexOfFragment).tail)
                alreadyAwaitingCustomersOrderedByOrderAwaiting ++ List(startingAwaitOrdByOrderAwaiting)
              }
              else if(indexOfFragment == -2) {
                val from2 = removeElemFrom(startingAwaitOrdByOrderAwaiting, bestNext)
                alreadyAwaitingCustomersOrderedByOrderAwaiting ++ List(from2)
              } else {
                //for -3 case when no awaiting customers at all
                alreadyAwaitingCustomersOrderedByOrderAwaiting
              } // length = 0?

            tmp3
          },
          restCustomers = futureCustomers,//.diff(List(bestNext)), //Rare case when we are finishing - futureCustomers.head
          accumulatorOfAllWaitingTimes = accumulatorOfAllWaitingTimes + currentWaitingTime)
    }

}

  val fragmentsOrdering = new Ordering[List[(Int, CustomersMeta)]] {
    override def compare(xs: List[(Int, CustomersMeta)], ys: List[(Int, CustomersMeta)]): Int = {
      (xs, ys) match {
        case (Nil, Nil) => 0
        case (Nil, ys) => 1 // reversed  if one list is emty to another one is min
        case (xs, Nil) => -1
        case (xs, ys) => Ordering[Int].compare(xs.head._1, ys.head._1)
      }
    }
  }

  private def getBestFrom(alreadyAwaiting : Array[List[(Int, CustomersMeta)]], startingAwait : List[(Int, CustomersMeta)], fun: (CustomersMeta,CustomersMeta) => Boolean): (CustomersMeta, Int) = {
    // We need our own implementation of `min` which return index as well
    val minFromAlreadySortedByEstimateFragment = Try { alreadyAwaiting.min(fragmentsOrdering) } match {
      case Success(res) => Some(res)
      case Failure(ex) =>  None
    }

    val minFromAlreadySortedByEstimate = minFromAlreadySortedByEstimateFragment.flatMap(_.headOption)
    val minFromStartingAwaitSortedByEstimate = startingAwait.headOption

    def indexOfMin = alreadyAwaiting.indexOf(minFromAlreadySortedByEstimateFragment.get)
      if (minFromAlreadySortedByEstimate.isDefined && minFromStartingAwaitSortedByEstimate.isDefined)
        if (fun(minFromAlreadySortedByEstimate.get._2,minFromStartingAwaitSortedByEstimate.get._2))
          (minFromAlreadySortedByEstimate.get._2, indexOfMin)
        else (minFromStartingAwaitSortedByEstimate.get._2, -2)
      else if (minFromAlreadySortedByEstimate.isEmpty && minFromStartingAwaitSortedByEstimate.isDefined)
        (minFromStartingAwaitSortedByEstimate.get._2, -2)
      else if (minFromAlreadySortedByEstimate.isDefined && minFromStartingAwaitSortedByEstimate.isEmpty)
        (minFromAlreadySortedByEstimate.get._2, indexOfMin)
      else {
        throw new IllegalStateException("How do we get here?")
      }
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

  def removeElemFrom(xs : List[(Int, CustomersMeta)], elem: CustomersMeta): List[(Int, CustomersMeta)] = {
    def loop(source: List[(Int, CustomersMeta)], acc: List[(Int, CustomersMeta)]): List[(Int, CustomersMeta)] = source match {
      case Nil => acc
      case head::tail =>
        if(head._2.id == elem.id) acc.reverse ::: tail
        else loop(tail, head::acc)
    }
    loop(xs, Nil)
  }


  private val firstCustomer: CustomersMeta = customersMeta.head
  val startTime = System.nanoTime()
  val minSum = calculateMinSumOfWaitingTime(0L, firstCustomer, new Array[List[(Int, CustomersMeta)]](0), new Array[List[(Int, CustomersMeta)]](0), customersMeta.tail)
  val finishTime = System.nanoTime()

  println(s"Calculation of average took ${(finishTime - startTime).toDouble / 1000000000} ")
  println(s"Calculation of totalMinWaiting took ${totalMinWaiting.toDouble / 1000000000} ")
  println(s"Calculation of totalMinWaitingMetas took ${totalMinWaitingMetas.toDouble / 1000000000} ")
  println(s"Calculation of totalTimeWaiting took ${totalTimeWaiting.toDouble / 1000000000} ")
  println(s"Calculation of totalBest took ${totalBest.toDouble / 1000000000} ")
  println(s"Calculation of total2 took ${total2.toDouble / 1000000000} ")
  println(s"Calculation of totalRemove2 took ${totalRemove2.toDouble / 1000000000} ")
  println(s"Calculation of total3 took ${total3.toDouble / 1000000000} ")

  println(s"Min sum of waiting time is: $minSum")

  println(s"Min avg of waiting time is: ${minSum / customersMeta.length}")

  val minSumPredef1 = calculateMinSumOfWaitingTime(0L, customersMetaPredef1.head, new Array[List[(Int, CustomersMeta)]](0), new Array[List[(Int, CustomersMeta)]](0), customersMetaPredef1.tail)

  println(s"==================================\nMin sum of waiting time in customersMetaPredef1 is: $minSumPredef1")

  println(s"Min avg of waiting time in customersMetaPredef1 is: ${minSumPredef1 / customersMetaPredef1.length}")
  system.shutdown()
}

case class CustomersMeta(id: Int, arrivalTime: Int, orderEstimate: Int, var waitingTime: Int = 0)