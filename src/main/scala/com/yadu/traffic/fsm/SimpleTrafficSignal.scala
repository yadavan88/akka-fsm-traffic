package com.yadu.traffic.fsm

import akka.actor.{Actor, ActorSystem, FSM, Props}
import scala.concurrent.ExecutionContext.Implicits.global
/**
  * Created by yadu on 17/11/16.
  */

sealed trait SimpleTrafficState

case object SimpleRedSignal extends SimpleTrafficState

case object SimpleYellowSignal extends SimpleTrafficState

case object SimpleGreenSignal extends SimpleTrafficState

sealed trait SimpleTrafficData

final case class SimpleNoData()  extends SimpleTrafficData

case object PressButton

class SimpleTrafficSignal extends Actor with FSM[SimpleTrafficState, SimpleTrafficData] {
  startWith(SimpleGreenSignal, SimpleNoData())

  when(SimpleGreenSignal) {
    case Event(PressButton, p: SimpleNoData) =>
      println("Received press event, change signal to YELLOW")
      goto(SimpleYellowSignal)

    case _ => stay()
  }

  when(SimpleYellowSignal) {
    case Event(PressButton, p: SimpleNoData) =>
      println("Received press event, change signal to RED")
      goto(SimpleRedSignal) using p
    case _                                  => stay()
  }

  when(SimpleRedSignal) {
    case Event(PressButton, p: SimpleNoData) =>
      println("Received press event, change signal to GREEN")
      goto(SimpleGreenSignal) using p
    case _                                  => stay()
  }
}

object SimpleTrafficSignalApp extends App {
  import scala.concurrent.duration._
  val system = ActorSystem()

  val ac = system.actorOf(Props(classOf[SimpleTrafficSignal]))
  system.scheduler.schedule(3.seconds,2.seconds,ac, PressButton)
}