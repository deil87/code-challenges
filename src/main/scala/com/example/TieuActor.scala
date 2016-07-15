package com.example

import akka.actor.{Actor, ActorLogging, Props}

class TieuActor extends Actor with ActorLogging {
  import TieuActor._

  var counter = 0

  def receive = {
  	case Initialize =>
	    log.info("In PingActor - starting ping-pong")

  }
}

object TieuActor {
  val props = Props[TieuActor]
  case object Initialize
  case class PingMessage(text: String)
}