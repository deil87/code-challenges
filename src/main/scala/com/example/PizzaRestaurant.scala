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

  customersMeta.foreach(cm => system.actorOf(CustomerActor.props(cm.id, cm.orderEstimate), "customerActor"+ cm.id))

  system.awaitTermination()
}

case class CustomersMeta(id: Int, arrivalTime: Int, orderEstimate: Int)