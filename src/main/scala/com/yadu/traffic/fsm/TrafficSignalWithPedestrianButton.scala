package com.yadu.traffic.fsm

import akka.actor.{Actor, ActorRef, ActorSystem, FSM, Props}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.io.StdIn

/**
  * Created by yadu on 17/11/16.
  */

sealed trait PedestrianTrafficState

case object RedSignal extends PedestrianTrafficState

case object YellowSignal extends PedestrianTrafficState

case object GreenSignal extends PedestrianTrafficState

case object PedestrianSignal extends PedestrianTrafficState

case class ButtonPressEvent(isPedestrianButton: Boolean)

case object PedestrianCrossEvent

sealed trait PedestrianTrafficData

case class ButtonPressInfo(hasAlreadyPressed: Boolean) extends PedestrianTrafficData

case class TrafficActorContainer(actorRef: ActorRef)

case object TIMEOUT

case class StartPedestrianMovement(actorRef: ActorRef)

class PedestrianCrossScheduler extends Actor {

  override def receive: Receive = {
    case actContainer: TrafficActorContainer =>
      println("Initiated the request for pedestrian crossing, wait for 5 seconds")
      context.system.scheduler.scheduleOnce(5.seconds, actContainer.actorRef, PedestrianCrossEvent)

    case cmd: StartPedestrianMovement => {
      println("Pedestrian are crossing the road")
      context.system.scheduler.scheduleOnce(10.seconds, cmd.actorRef, TIMEOUT)
    }

    case any => println("Received this message : " + any)
  }

}

class PedestrianTrafficSignal extends Actor with FSM[PedestrianTrafficState, ButtonPressInfo] {
  var hasAlreadyPressed: Boolean = false
  val schedulerActor = context.system.actorOf(Props(classOf[PedestrianCrossScheduler]))

  startWith(RedSignal, ButtonPressInfo(false))

  when(GreenSignal) {

    case Event(b: ButtonPressEvent, p: ButtonPressInfo) if b.isPedestrianButton => {
      println("Someone has pressed Pedestrian button.")
      if (hasAlreadyPressed == true) {
        //already pressed, waiting for the signal to turn red. Do not initiate again
        println("Already received the request, please wait for some time")
        stay()
      } else {
        //send to the scheduler and wait for the timeout
        schedulerActor ! TrafficActorContainer(self)
        hasAlreadyPressed = true
        stay()
      }

    }

    case Event(b: ButtonPressEvent, _) => println("Received press event, change signal to Yellow"); goto(YellowSignal)

    case Event(PedestrianCrossEvent, _) => println("Timeout from scheduler, allow pedestrians to cross"); schedulerActor ! StartPedestrianMovement(self); goto(PedestrianSignal)

    case any => println("invalid event =>" + any); stay()
  }

  when(PedestrianSignal) {
    case Event(TIMEOUT, _) => println("Crossing time over, Stop pedestrian movement"); hasAlreadyPressed = false; goto(GreenSignal)
    case any               => println("No events allowed until pedestrian crosses"); stay()
  }

  when(YellowSignal) {
    case Event(e: ButtonPressEvent, b: ButtonPressInfo) if e.isPedestrianButton == false =>
      println("Received press event, change signal to RED")
      goto(RedSignal)
    case Event(e: ButtonPressEvent, b: ButtonPressInfo) if e.isPedestrianButton == true  =>
      println("Pedestrian button ignored since the signal is in YELLOW")
      stay()
    case any                                                                             => println("invalid event ->" + any); stay()
  }

  when(RedSignal) {
    case Event(e: ButtonPressEvent, _) if e.isPedestrianButton == false =>
      println("Received press event, change signal to GREEN")
      goto(GreenSignal)
    case Event(e: ButtonPressEvent, _) if e.isPedestrianButton == true  =>
      println("Pedestrian button ignored since the signal is already RED")
      stay()
    case _                                                              => stay()
  }

}

object PedestrianTrafficSignalApp extends App {

  import scala.concurrent.duration._

  val system = ActorSystem()

  val ac = system.actorOf(Props(classOf[PedestrianTrafficSignal]))

  system.scheduler.schedule(3.seconds, 10.seconds, ac, ButtonPressEvent(false))

  while (true) {
    val option = StdIn.readInt()
    if (option == 0) {
      //pedestrian button pressed
      ac ! ButtonPressEvent(true)
    }
  }
}
