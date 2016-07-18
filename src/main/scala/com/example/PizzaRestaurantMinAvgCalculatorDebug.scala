package com.example

import akka.actor.ActorSystem

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

object PizzaRestaurantMinAvgCalculatorDebug extends App with RestaurantConstraints with RestaurantOrderingHelper with MagicNumbers{
  val system = ActorSystem("PizzaRestaurantSystem")


  var totalGetBests = 0L
  var totalGetSize = 0L
  var totalTimeWaiting = 0L
  var totalBest = 0L
  var totalAwaitingConcat = 0L
  var totalMinFinding  = 0L
  var totalIndexOf  = 0L
  var totalForeach  = 0L

  def resetTiming()  {
    totalGetBests = 0L
    totalGetSize = 0L
    totalTimeWaiting = 0L
    totalBest = 0L
    totalAwaitingConcat = 0L
    totalMinFinding  = 0L
    totalIndexOf  = 0L
    totalForeach  = 0L
  }

  def calculateMinAvgWaitingTime(customerMetas: List[CustomerMeta]): Long = {
    val startTime = System.nanoTime()

    val minSum = calculateMinSumOfWaitingTime(0L, customerMetas.head, new Array[List[(Int, CustomerMeta)]](0), new Array[List[(Int, CustomerMeta)]](0), customerMetas.tail)

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

    val minAverage: Long = minSum / customerMetas.length
    println(s"Min avg of waiting time is: $minAverage")
    minAverage
  }

  @tailrec
  private def calculateMinSumOfWaitingTime(totalServedTime: Long,
                                           currentCustomer: CustomerMeta,
                                           alreadyAwaitingCustomersOrderedByOrderEstimate: Array[List[(Int, CustomerMeta)]],
                                           alreadyAwaitingCustomersOrderedByOrderAwaiting: Array[List[(Int, CustomerMeta)]],
                                           restCustomers: List[CustomerMeta],
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
      if (startingAwaitOrdByOrderEstimate.isEmpty && alreadyAwaitingCustomersCount == 0L )
        (futureCustomers.head, pickingUpFromRestCustomersMagicN) // case when we just getting next
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
            startingAwaitOrdByOrderEstimate.par.foreach{case (v, cm) =>
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
              else if(indexOfFragment == pickingUpFromStartingAwaitMagicN) {
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
            startingAwaitOrdByOrderAwaiting.par.foreach{case (v, cm) =>
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
              else if(indexOfFragment == pickingUpFromStartingAwaitMagicN) {
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


  /**
    *
    * @return tuple of best next customer for the min impact on total awaiting time and indexOfFragment;
    *         indexOfFragment is index of sorted sublist where we have found the bestNext.
    *         We set it to `-2` in case we picking up from just starting to wait customers.
    *         We set it to `-3` in case there is no awaiting and we should take next from restCustomers(Nobody is awaiting)
    *
    */
  private def getBestFrom(alreadyAwaiting : Array[List[(Int, CustomerMeta)]], startingAwait : List[(Int, CustomerMeta)], fun: (CustomerMeta,CustomerMeta) => Boolean): (CustomerMeta, Int) = {
    // Better use our own implementation of `min` which returns index as well
    val startMin = System.nanoTime()

    //maybe keep alreadyAwaiting fragments sorted?
    val minFromAlreadySortedByEstimateFragment = Try { alreadyAwaiting.min(fragmentsOrdering) } match {
      case Success(res) => Some(res)
      case Failure(ex) =>  {
        None
      }
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
      else (minFromStartingAwaitSortedByEstimate.get._2, pickingUpFromStartingAwaitMagicN)
    else if (minFromAlreadySortedByEstimate.isEmpty && minFromStartingAwaitSortedByEstimate.isDefined)
      (minFromStartingAwaitSortedByEstimate.get._2, pickingUpFromStartingAwaitMagicN)
    else if (minFromAlreadySortedByEstimate.isDefined && minFromStartingAwaitSortedByEstimate.isEmpty)
      (minFromAlreadySortedByEstimate.get._2, indexOfMin)
    else if(minFromAlreadySortedByEstimate.isEmpty && minFromStartingAwaitSortedByEstimate.isEmpty)
    {
      val tmp = Try { alreadyAwaiting.min(fragmentsOrdering) } match {
        case Success(res) => Some(res)
        case Failure(ex) =>  {
          val err = ex
          None
        }
      }
      throw new IllegalStateException("How do we get here?")
    }
    else {
      throw new IllegalStateException("How do we get here?")
    }
    val finishIndexOf = System.nanoTime()
    totalIndexOf = totalIndexOf + (finishIndexOf - startIndexOf)

    res
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

  def removeElemFrom(xs : List[(Int, CustomerMeta)], elem: CustomerMeta): List[(Int, CustomerMeta)] = {
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

