package com.example

import akka.actor.ActorSystem
import akka.actor.Actor
import akka.actor.Props
import akka.testkit.{ImplicitSender, TestActors, TestKit}
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach, Matchers, WordSpecLike}
 
class PizzaRestaurantMinAvgCalculatorSpec(_system: ActorSystem) extends TestKit(_system) with ImplicitSender
  with WordSpecLike with Matchers with BeforeAndAfterAll with BeforeAndAfterEach with RestaurantConstraints{
 
  def this() = this(ActorSystem("MySpec"))

  override def afterAll {
    TestKit.shutdownActorSystem(system)
  }

 
  "PizzaRestaurantMinAvgCalculator" must {
    "calculate average waiting time for example data" in {
      PizzaRestaurantMinAvgCalculatorDebug.resetTiming()
      val customersMetaPredef1 = List(CustomerMeta(0,0, 3), CustomerMeta(1,1, 9), CustomerMeta(2,2, 5))
      assert(PizzaRestaurantMinAvgCalculatorDebug.calculateMinAvgWaitingTime(customersMetaPredef1) == 8)
    }

    "calculate average waiting time for example2 data" in {
      PizzaRestaurantMinAvgCalculatorDebug.resetTiming()
      val customersMetaPredef1 = List(CustomerMeta(0,0, 3), CustomerMeta(1,1, 9), CustomerMeta(2,2, 6))
      assert(PizzaRestaurantMinAvgCalculatorDebug.calculateMinAvgWaitingTime(customersMetaPredef1) == 9)
    }

    "calculate average waiting time with up to constraints data" in {
      val customerMetas = (0 until numberOfCustomers)
        .map(i => CustomerMeta(i, randomArrivalTime, orderEstimate = randomPizzaTimeCost))
        .sortBy(_.arrivalTime).toList

      var totalTime = 0L
      val startTime = System.nanoTime()

      val minAverage = PizzaRestaurantMinAvgCalculator.calculateMinAvgWaitingTime(customerMetas)

      val finishTime = System.nanoTime()
      totalTime = totalTime + (finishTime - startTime)

      println(s"\nCalculation of min average took ${totalTime.toDouble / 1000000000} ")

      println(s"Min avg of waiting time for up to constraints data is: $minAverage")
      assert(minAverage > 0)

    }

    "calculate average waiting time with up to constraints data(debug)" in {
      PizzaRestaurantMinAvgCalculatorDebug.resetTiming()
      val customerMetas = (0 until numberOfCustomers)
        .map(i => CustomerMeta(i, randomArrivalTime, orderEstimate = randomPizzaTimeCost))
        .sortBy(_.arrivalTime).toList

      val minAverage: Long = PizzaRestaurantMinAvgCalculatorDebug.calculateMinAvgWaitingTime(customerMetas)

      println(s"Min avg of waiting time for up to constraints data(debug version) is: $minAverage")
      assert(minAverage > 0)

    }
  }

  override protected def beforeEach(): Unit = {
    println("\n")
  }
}
