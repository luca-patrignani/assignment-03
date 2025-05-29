package it.unibo.pcd.akka.basics.e01hello

import akka.actor.{Actor, ActorLogging, ActorRef}
import akka.actor.Actor.Receive

class MyActor(val tag: String) extends Actor with ActorLogging: // actors extends from an Actor trait (untyped)
  def receive: Receive = {
    case "ping" => sender() ! ("pong from: " + tag)
    case pong => println(pong)
  }

object MyActor:
  def apply(tag: String): MyActor = new MyActor(tag) // factory method

def inContextOf(actor: ActorRef)(block: ActorRef ?=> Unit): Unit = // using clause
  given ActorRef = actor
  block
@main def main(): Unit =
  val system = akka.actor.ActorSystem("factory") // factory for actors
  Thread.sleep(1000) // wait for the actors to respond
  val actor1 = system.actorOf(akka.actor.Props(MyActor("actor 1")), "actor1") // create an actor
  val actor2 = system.actorOf(akka.actor.Props(MyActor("actor 2")), "actor2") // create another actor
  inContextOf(actor2):
    actor1 ! "ping" // send a message to actor1
  inContextOf(actor1):
    actor2 ! "ping" // send a message to actor2
  Thread.sleep(1000) // wait for the actors to respond
  system.terminate() // terminate the factory
