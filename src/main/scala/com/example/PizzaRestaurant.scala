package com.example

import java.util

import akka.actor.ActorSystem

import scala.annotation.tailrec
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Random, Success, Try}

object PizzaRestaurant extends App {
  val system = ActorSystem("PizzaRestaurantSystem")

  val numberOfCustomers = 10000 // Really popular restaurant!

  // Note: randomPizzaTimeCost and randomArrivalTime should be of similar order. If arrival time >> pizza cooking time, Tieu will be bored (and beggar).
  def randomArrivalTime = Random.nextInt(1000000001)

  def randomPizzaTimeCost = Random.nextInt(1000000000) + 1

  val customersMeta = (0 until numberOfCustomers)
    .map(i => CustomersMeta(i, randomArrivalTime, orderEstimate = randomPizzaTimeCost))
    .sortBy(_.arrivalTime).toList

  val customersMetaPredef1 = List(CustomersMeta(0,0, 3), CustomersMeta(1,1, 9), CustomersMeta(2,2, 5))

  var totalGetBests = 0L
  var totalGetSize = 0L
  var totalTimeWaiting = 0L
  var totalBest = 0L
  var totalAwaitingConcat = 0L
  var totalMinFinding  = 0L
  var totalIndexOf  = 0L
  var totalForeach  = 0L

  @tailrec
  private def calculateMinSumOfWaitingTime(totalServedTime: Long,
                                           currentCustomer: CustomersMeta,
                                           alreadyAwaitingCustomersOrderedByOrderEstimate: Array[List[(Int, CustomersMeta)]],
                                           alreadyAwaitingCustomersOrderedByOrderAwaiting: Array[List[(Int, CustomersMeta)]],
                                           restCustomers: List[CustomersMeta],
                                           accumulatorOfAllWaitingTimes: Long = 0L,
                                           alreadyAwaitingCustomersCount: Long = 0L): Long = {

  val startTimeWaiting = System.nanoTime()
    val withCurrentTotalServedTime = currentCustomer.orderEstimate.toLong + totalServedTime
    val currentWaitingTime =
      if (currentCustomer.arrivalTime.toLong > totalServedTime) currentCustomer.orderEstimate.toLong + currentCustomer.waitingTime // ??currentCustomer.waitingTime
      else withCurrentTotalServedTime - currentCustomer.arrivalTime // ?? + currentCustomer.waitingTime
  val finishTimeWaiting = System.nanoTime()
  totalTimeWaiting = totalTimeWaiting + (finishTimeWaiting - startTimeWaiting)

  if(alreadyAwaitingCustomersCount == 0L && restCustomers.isEmpty)
    accumulatorOfAllWaitingTimes + currentWaitingTime
  else {
    val startingAwaitCustomers = restCustomers.takeWhile(_.arrivalTime <= currentCustomer.arrivalTime + currentCustomer.orderEstimate)

    val  futureCustomers = restCustomers.drop(startingAwaitCustomers.length)

    val startingAwaitOrdByOrderEstimate = startingAwaitCustomers.sortBy(_.orderEstimate).map(cm => (cm.orderEstimate, cm))
    val startingAwaitOrdByOrderAwaiting = startingAwaitCustomers.map(cm => (cm.waitingTime, cm))

    val startGetSize = System.nanoTime()
    val amountOfWaitingCustomersOnNextStep = alreadyAwaitingCustomersCount + startingAwaitOrdByOrderEstimate.length - 1
    val finishGetSize = System.nanoTime()
    totalGetSize = totalGetSize + (finishGetSize - startGetSize)

    val startBest = System.nanoTime()
    val (bestNext, indexOfFragment) =
      if (startingAwaitOrdByOrderEstimate.isEmpty && alreadyAwaitingCustomersOrderedByOrderEstimate.isEmpty )
        (futureCustomers.head, -3) // case when we just getting next
      else {
        val startGetBests = System.nanoTime()
        val withMinOrderEstimateImpactToOthers = getBestFrom(alreadyAwaitingCustomersOrderedByOrderEstimate, startingAwaitOrdByOrderEstimate, _.orderEstimate < _.orderEstimate)

        val withCurrentMaxWaitingTime = getBestFrom(alreadyAwaitingCustomersOrderedByOrderAwaiting, startingAwaitOrdByOrderAwaiting, _.waitingTime > _.waitingTime)

        val finishGetBests = System.nanoTime()

        totalGetBests = totalGetBests + (finishGetBests - startGetBests)

        if ((withMinOrderEstimateImpactToOthers._1.waitingTime + withMinOrderEstimateImpactToOthers._1.orderEstimate * amountOfWaitingCustomersOnNextStep) < withCurrentMaxWaitingTime._1.waitingTime + withCurrentMaxWaitingTime._1.orderEstimate * amountOfWaitingCustomersOnNextStep) {
          withMinOrderEstimateImpactToOthers
        }
        else {
          withCurrentMaxWaitingTime
        }
      }
    val finishBest = System.nanoTime()

    totalBest = totalBest + (finishBest - startBest)

    val waitingDelta = bestNext.orderEstimate + currentCustomer.orderEstimate + currentCustomer.arrivalTime



    calculateMinSumOfWaitingTime(totalServedTime = withCurrentTotalServedTime,
          currentCustomer = bestNext,
          alreadyAwaitingCustomersOrderedByOrderEstimate = {

            val orderEstimate = bestNext.orderEstimate

            val startForeach = System.nanoTime()

            alreadyAwaitingCustomersOrderedByOrderEstimate.par.foreach { fs =>
              fs.foreach { case (v, cm) =>
                cm.waitingTime += orderEstimate
              }
            }

            val finishForeach = System.nanoTime()

            totalForeach = totalForeach + (finishForeach - startForeach)
            startingAwaitOrdByOrderEstimate.foreach{case (v, cm) =>
              val delta = waitingDelta - cm.arrivalTime
              cm.waitingTime += delta
            }

            val startAwaitingConcat = System.nanoTime()

            val tmp2 =
              if(indexOfFragment >= 0) {
                alreadyAwaitingCustomersOrderedByOrderEstimate.update(indexOfFragment, alreadyAwaitingCustomersOrderedByOrderEstimate(indexOfFragment).tail)
                if(startingAwaitOrdByOrderEstimate.isEmpty)
                  alreadyAwaitingCustomersOrderedByOrderEstimate
                else
                  alreadyAwaitingCustomersOrderedByOrderEstimate ++ List(startingAwaitOrdByOrderEstimate)
              }
              else if(indexOfFragment == -2) {
                val from2 = removeElemFrom(startingAwaitOrdByOrderEstimate, bestNext)
                alreadyAwaitingCustomersOrderedByOrderEstimate ++ List(from2)
              } else {
                //for -3 case when no awaiting customers at all
                alreadyAwaitingCustomersOrderedByOrderAwaiting
              }
            val finishAwaitingConcat = System.nanoTime()

            totalAwaitingConcat = totalAwaitingConcat + (finishAwaitingConcat - startAwaitingConcat)

            tmp2
          },
          alreadyAwaitingCustomersOrderedByOrderAwaiting = {

            val startForeach = System.nanoTime()

            val orderEstimate = bestNext.orderEstimate
            alreadyAwaitingCustomersOrderedByOrderAwaiting.par.foreach { fs =>
              fs.foreach { case (v, cm) =>
                cm.waitingTime += orderEstimate
              }
            }

            val finishForeach = System.nanoTime()

            totalForeach = totalForeach + (finishForeach - startForeach)
            startingAwaitOrdByOrderAwaiting.foreach{case (v, cm) =>
              val delta = waitingDelta - cm.arrivalTime
              cm.waitingTime += delta
            }


            val startAwaitingConcat = System.nanoTime()

            val tmp3 =
              if(indexOfFragment >= 0) {
                alreadyAwaitingCustomersOrderedByOrderAwaiting.update(indexOfFragment, alreadyAwaitingCustomersOrderedByOrderAwaiting(indexOfFragment).tail)
                if(startingAwaitOrdByOrderAwaiting.isEmpty)
                  alreadyAwaitingCustomersOrderedByOrderAwaiting
                else
                  alreadyAwaitingCustomersOrderedByOrderAwaiting ++ List(startingAwaitOrdByOrderAwaiting)
              }
              else if(indexOfFragment == -2) {
                val from2 = removeElemFrom(startingAwaitOrdByOrderAwaiting, bestNext)
                alreadyAwaitingCustomersOrderedByOrderAwaiting ++ List(from2)
              } else {
                //for -3 case when no awaiting customers at all
                alreadyAwaitingCustomersOrderedByOrderAwaiting
              }
            val finishAwaitingConcat = System.nanoTime()

            totalAwaitingConcat = totalAwaitingConcat + (finishAwaitingConcat - startAwaitingConcat)

            tmp3
          },
          restCustomers = futureCustomers,
          accumulatorOfAllWaitingTimes = accumulatorOfAllWaitingTimes + currentWaitingTime,
      alreadyAwaitingCustomersCount = amountOfWaitingCustomersOnNextStep)
    }

}

  val fragmentsOrdering = new Ordering[List[(Int, CustomersMeta)]] {
    override def compare(xs: List[(Int, CustomersMeta)], ys: List[(Int, CustomersMeta)]): Int = {
      (xs, ys) match {
        case (Nil, Nil) => 0
        case (Nil, ys) => 1 // reversed  if one list is empty then another one is returned
        case (xs, Nil) => -1
        case (xs, ys) => Ordering[Int].compare(xs.head._1, ys.head._1)
      }
    }
  }

  private def getBestFrom(alreadyAwaiting : Array[List[(Int, CustomersMeta)]], startingAwait : List[(Int, CustomersMeta)], fun: (CustomersMeta,CustomersMeta) => Boolean): (CustomersMeta, Int) = {
    // We need our own implementation of `min` which return index as well
    val startMin = System.nanoTime()

    //maybe keep alreadyAwaiting fragments sorted?
    val minFromAlreadySortedByEstimateFragment = Try { alreadyAwaiting.min(fragmentsOrdering) } match {
      case Success(res) => Some(res)
      case Failure(ex) =>  None
    }
    val finishMin = System.nanoTime()
    totalMinFinding = totalMinFinding + (finishMin - startMin)

    val minFromAlreadySortedByEstimate = minFromAlreadySortedByEstimateFragment.flatMap(_.headOption)
    val minFromStartingAwaitSortedByEstimate = startingAwait.headOption

    def indexOfMin = alreadyAwaiting.indexOf(minFromAlreadySortedByEstimateFragment.get)

    val startIndexOf = System.nanoTime()
    val res = if (minFromAlreadySortedByEstimate.isDefined && minFromStartingAwaitSortedByEstimate.isDefined)
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
    val finishIndexOf = System.nanoTime()
    totalIndexOf = totalIndexOf + (finishIndexOf - startIndexOf)

    res
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
  println(s"Calculation of totalGetBests took ${totalGetBests.toDouble / 1000000000} ")
  println(s"Calculation of totalGetSize took ${totalGetSize.toDouble / 1000000000} ")
  println(s"Calculation of totalTimeWaiting took ${totalTimeWaiting.toDouble / 1000000000} ")
  println(s"Calculation of totalBest took ${totalBest.toDouble / 1000000000} ")
  println(s"Calculation of totalAwaitingConcat took ${totalAwaitingConcat.toDouble / 1000000000} ")
  println(s"Calculation of totalIndexOf took ${totalIndexOf.toDouble / 1000000000} ")
  println(s"Calculation of totalMinFinding took ${totalMinFinding.toDouble / 1000000000} ")
  println(s"Calculation of totalForeach took ${totalForeach.toDouble / 1000000000} ")

  println(s"Min sum of waiting time is: $minSum")

  println(s"Min avg of waiting time is: ${minSum / customersMeta.length}")

  val minSumPredef1 = calculateMinSumOfWaitingTime(0L, customersMetaPredef1.head, new Array[List[(Int, CustomersMeta)]](0), new Array[List[(Int, CustomersMeta)]](0), customersMetaPredef1.tail)

  println(s"==================================\nMin sum of waiting time in customersMetaPredef1 is: $minSumPredef1")

  println(s"Min avg of waiting time in customersMetaPredef1 is: ${minSumPredef1 / customersMetaPredef1.length}")
  system.shutdown()
}

case class CustomersMeta(id: Int, arrivalTime: Int, orderEstimate: Int, var waitingTime: Int = 0)