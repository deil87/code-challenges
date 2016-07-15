package com.example

import akka.actor.{Actor, ActorLogging, Props}

class CustomerActor(id: Int, orderEstimate: Int) extends Actor with ActorLogging {
  import CustomerActor._


  override def preStart(): Unit = {
    log.info(s"Consumer $id with orderEstimateis = $orderEstimate is started")
    super.preStart()
  }

  def receive = {
  	case TieuActor.PingMessage(text) =>
  	  log.info("In PongActor - received message: {}", text)
  }
}

object CustomerActor {
  def props(id: Int, orderEstimate: Int) = Props(classOf[CustomerActor], id, orderEstimate)
}
