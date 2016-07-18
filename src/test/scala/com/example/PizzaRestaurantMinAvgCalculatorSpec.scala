package com.example

import akka.actor.ActorSystem
import akka.actor.Actor
import akka.actor.Props
import akka.testkit.{ TestActors, TestKit, ImplicitSender }
import org.scalatest.WordSpecLike
import org.scalatest.Matchers
import org.scalatest.BeforeAndAfterAll
 
class PizzaRestaurantMinAvgCalculatorSpec(_system: ActorSystem) extends TestKit(_system) with ImplicitSender
  with WordSpecLike with Matchers with BeforeAndAfterAll with RestaurantConstraints{
 
  def this() = this(ActorSystem("MySpec"))

  override def afterAll {
    TestKit.shutdownActorSystem(system)
  }
 
  "PizzaRestaurantMinAvgCalculator" must {
    "calculate average waiting time for example data" in {
      val customersMetaPredef1 = List(CustomerMeta(0,0, 3), CustomerMeta(1,1, 9), CustomerMeta(2,2, 5))
      PizzaRestaurantMinAvgCalculator.calculateMinAvgWaitingTime(customersMetaPredef1) === 8
    }

    "calculate average waiting time with up to constraints data" in {

      val customerMetas = (0 until numberOfCustomers)
        .map(i => CustomerMeta(i, randomArrivalTime, orderEstimate = randomPizzaTimeCost))
        .sortBy(_.arrivalTime).toList

      assert(PizzaRestaurantMinAvgCalculator.calculateMinAvgWaitingTime(customerMetas) > 0)

    }
  }

}
