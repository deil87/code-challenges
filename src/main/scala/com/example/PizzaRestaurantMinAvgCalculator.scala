package com.example

import java.util

import akka.actor.ActorSystem

import scala.annotation.tailrec
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Random, Success, Try}

object PizzaRestaurantMinAvgCalculator extends App with RestaurantConstraints with RestaurantOrderingHelper with MagicNumbers {
  val system = ActorSystem("PizzaRestaurantSystem")

  def calculateMinAvgWaitingTime(customerMetas: List[CustomerMeta]): Long = {

    val minSum = calculateMinSumOfWaitingTime(0L, customerMetas.head, new Array[List[(Int, CustomerMeta)]](0), new Array[List[(Int, CustomerMeta)]](0), customerMetas.tail)

    minSum / customerMetas.length
  }



  @tailrec
  private def calculateMinSumOfWaitingTime(totalServedTime: Long,
                                           currentCustomer: CustomerMeta,
                                           alreadyAwaitingCustomersOrderedByOrderEstimate: Array[List[(Int, CustomerMeta)]],
                                           alreadyAwaitingCustomersOrderedByOrderAwaiting: Array[List[(Int, CustomerMeta)]],
                                           restCustomers: List[CustomerMeta],
                                           accumulatorOfAllWaitingTimes: Long = 0L,
                                           alreadyAwaitingCustomersCount: Long = 0L): Long = {

    val withCurrentTotalServedTime = currentCustomer.orderEstimate.toLong + totalServedTime
    val currentWaitingTime =
      if (currentCustomer.arrivalTime.toLong > totalServedTime)
        currentCustomer.orderEstimate.toLong + currentCustomer.waitingTime
      else withCurrentTotalServedTime - currentCustomer.arrivalTime // ?? + currentCustomer.waitingTime

  if(alreadyAwaitingCustomersCount == 0L && restCustomers.isEmpty)
    accumulatorOfAllWaitingTimes + currentWaitingTime
  else {
    val startingAwaitCustomers = restCustomers.takeWhile(_.arrivalTime <= currentCustomer.arrivalTime + currentCustomer.orderEstimate)

    val  futureCustomers = restCustomers.drop(startingAwaitCustomers.length)

    val startingAwaitOrdByOrderEstimate = startingAwaitCustomers.sortBy(_.orderEstimate).map(cm => (cm.orderEstimate, cm))
    val startingAwaitOrdByOrderAwaiting = startingAwaitCustomers.map(cm => (cm.waitingTime, cm))

    val amountOfWaitingCustomersOnNextStep = alreadyAwaitingCustomersCount + startingAwaitOrdByOrderEstimate.length - 1

    val (bestNext, indexOfFragment) =
      if (startingAwaitOrdByOrderEstimate.isEmpty && alreadyAwaitingCustomersCount == 0L )
        (futureCustomers.head, pickingUpFromRestCustomersMagicN) // case when we just getting next of restCustomers
      else {
        val withMinOrderEstimateImpactToOthers = getBestNextWithMinWaitingImpactFrom(alreadyAwaitingCustomersOrderedByOrderEstimate, startingAwaitOrdByOrderEstimate, _.orderEstimate < _.orderEstimate)

        val withCurrentMaxWaitingTime = getBestNextWithMinWaitingImpactFrom(alreadyAwaitingCustomersOrderedByOrderAwaiting, startingAwaitOrdByOrderAwaiting, _.waitingTime > _.waitingTime)

        if ((withMinOrderEstimateImpactToOthers._1.waitingTime + withMinOrderEstimateImpactToOthers._1.orderEstimate * amountOfWaitingCustomersOnNextStep) < withCurrentMaxWaitingTime._1.waitingTime + withCurrentMaxWaitingTime._1.orderEstimate * amountOfWaitingCustomersOnNextStep) {
          withMinOrderEstimateImpactToOthers
        }
        else {
          withCurrentMaxWaitingTime
        }
      }

    val waitingDelta = bestNext.orderEstimate + currentCustomer.orderEstimate + currentCustomer.arrivalTime

    calculateMinSumOfWaitingTime(
      totalServedTime = withCurrentTotalServedTime,
      currentCustomer = bestNext,
      alreadyAwaitingCustomersOrderedByOrderEstimate = {

        val orderEstimate = bestNext.orderEstimate

        alreadyAwaitingCustomersOrderedByOrderEstimate.par.foreach { fs =>
          fs.foreach { case (v, cm) =>
            cm.waitingTime += orderEstimate
          }
        }

        startingAwaitOrdByOrderEstimate.par.foreach { case (v, cm) =>
          val delta = waitingDelta - cm.arrivalTime
          cm.waitingTime += delta
        }

        if (indexOfFragment >= 0) {
          alreadyAwaitingCustomersOrderedByOrderEstimate.update(indexOfFragment, alreadyAwaitingCustomersOrderedByOrderEstimate(indexOfFragment).tail)
          if (startingAwaitOrdByOrderEstimate.isEmpty)
            alreadyAwaitingCustomersOrderedByOrderEstimate
          else
            alreadyAwaitingCustomersOrderedByOrderEstimate ++ List(startingAwaitOrdByOrderEstimate)
        }
        else if (indexOfFragment == pickingUpFromStartingAwaitMagicN) {
          val from2 = removeElemFrom(startingAwaitOrdByOrderEstimate, bestNext)
          alreadyAwaitingCustomersOrderedByOrderEstimate ++ List(from2)
        } else {
          //for -3 case when no awaiting customers at all
          alreadyAwaitingCustomersOrderedByOrderAwaiting
        }
      },
      alreadyAwaitingCustomersOrderedByOrderAwaiting = {

        val orderEstimate = bestNext.orderEstimate
        alreadyAwaitingCustomersOrderedByOrderAwaiting.par.foreach { fs =>
          fs.foreach { case (v, cm) =>
            cm.waitingTime += orderEstimate
          }
        }
        startingAwaitOrdByOrderAwaiting.par.foreach { case (v, cm) =>
          val delta = waitingDelta - cm.arrivalTime
          cm.waitingTime += delta
        }

        if (indexOfFragment >= 0) {
          alreadyAwaitingCustomersOrderedByOrderAwaiting.update(indexOfFragment, alreadyAwaitingCustomersOrderedByOrderAwaiting(indexOfFragment).tail)
          if (startingAwaitOrdByOrderAwaiting.isEmpty)
            alreadyAwaitingCustomersOrderedByOrderAwaiting
          else
            alreadyAwaitingCustomersOrderedByOrderAwaiting ++ List(startingAwaitOrdByOrderAwaiting)
        }
        else if (indexOfFragment == pickingUpFromStartingAwaitMagicN) {
          val from2 = removeElemFrom(startingAwaitOrdByOrderAwaiting, bestNext)
          alreadyAwaitingCustomersOrderedByOrderAwaiting ++ List(from2)
        } else {
          //for -3 case when no awaiting customers at all
          alreadyAwaitingCustomersOrderedByOrderAwaiting
        }
      },
      restCustomers = futureCustomers,
      accumulatorOfAllWaitingTimes = accumulatorOfAllWaitingTimes + currentWaitingTime,
      alreadyAwaitingCustomersCount = amountOfWaitingCustomersOnNextStep
    )
    }

}

  /**
    *
    * @return tuple of best next customer for the min impact on total awaiting time and indexOfFragment;
    *         indexOfFragment is index of sorted sublist where we have found the bestNext.
    *         We set it to `-2` in case we picking up from just starting to wait customers.
    *         We set it to `-3` in case there is no awaiting and we should take next from restCustomers(Nobody is awaiting)
    *
    */
  private def getBestNextWithMinWaitingImpactFrom(alreadyAwaiting : Array[List[(Int, CustomerMeta)]], startingAwait : List[(Int, CustomerMeta)], fun: (CustomerMeta,CustomerMeta) => Boolean): (CustomerMeta, Int) = {
    //maybe keep alreadyAwaiting fragments sorted?
    val minFromAlreadySortedByEstimateFragment = Try { alreadyAwaiting.min(fragmentsOrdering) } match {
      case Success(minFragment) => Some(minFragment)
      case Failure(ex) => None
    }

    val minFromAlreadySortedByEstimate = minFromAlreadySortedByEstimateFragment.flatMap(_.headOption)
    val minFromStartingAwaitSortedByEstimate = startingAwait.headOption

    // Better use our own implementation of `min` which returns index as well
    def indexOfMin = alreadyAwaiting.indexOf(minFromAlreadySortedByEstimateFragment.get)

    if (minFromAlreadySortedByEstimate.isDefined && minFromStartingAwaitSortedByEstimate.isDefined)
      if (fun(minFromAlreadySortedByEstimate.get._2,minFromStartingAwaitSortedByEstimate.get._2))
        (minFromAlreadySortedByEstimate.get._2, indexOfMin)
      else (minFromStartingAwaitSortedByEstimate.get._2, pickingUpFromStartingAwaitMagicN)
    else if (minFromAlreadySortedByEstimate.isEmpty && minFromStartingAwaitSortedByEstimate.isDefined)
      (minFromStartingAwaitSortedByEstimate.get._2, pickingUpFromStartingAwaitMagicN)
    else if (minFromAlreadySortedByEstimate.isDefined && minFromStartingAwaitSortedByEstimate.isEmpty)
      (minFromAlreadySortedByEstimate.get._2, indexOfMin)
    else {
      throw new IllegalStateException("How do we get here?")
    }
  }

  private def merge(xs : List[CustomerMeta], ys : List[CustomerMeta], fun: (CustomerMeta,CustomerMeta) => Boolean) : List[CustomerMeta] = {
    (xs, ys) match{
      case (Nil, Nil) => Nil
      case (Nil, ys) => ys
      case (xs, Nil) => xs
      case (x :: xs1, y :: ys1) =>
        if(fun(x,y)) x :: merge(xs1, ys, fun)
        else y :: merge(xs, ys1, fun)
    }
  }

  private def removeElemFrom(xs : List[(Int, CustomerMeta)], elem: CustomerMeta): List[(Int, CustomerMeta)] = {
    def loop(source: List[(Int, CustomerMeta)], acc: List[(Int, CustomerMeta)]): List[(Int, CustomerMeta)] = source match {
      case Nil => acc
      case head::tail =>
        if(head._2.id == elem.id) acc.reverse ::: tail
        else loop(tail, head::acc)
    }
    loop(xs, Nil)
  }

  system.shutdown()
}

case class CustomerMeta(id: Int, arrivalTime: Int, orderEstimate: Int, var waitingTime: Int = 0)