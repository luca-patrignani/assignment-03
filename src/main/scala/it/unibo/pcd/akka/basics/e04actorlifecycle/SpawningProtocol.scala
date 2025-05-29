package it.unibo.pcd.akka.basics.e04actorlifecycle

import akka.actor.typed.{ActorRef, ActorSystem, Behavior, Props, Scheduler, SpawnProtocol}
import akka.actor.typed.scaladsl.Behaviors
import akka.util.Timeout

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.DurationInt

object SpawningProtocol extends App:
  case object Ping
  case object Pong

  val system = ActorSystem(Behaviors.setup[SpawnProtocol.Command](_ => SpawnProtocol()), "myroot")

  import akka.actor.typed.scaladsl.AskPattern.*
  given ExecutionContext = system.executionContext
  given Scheduler = system.scheduler // for ask pattern
  given Timeout = Timeout(3.seconds) // for ask pattern

  def pongerBehavior: Behavior[Ping.type] = Behaviors.receive[Ping.type]: (ctx, _) =>
    ctx.log.info("pong"); Behaviors.stopped
  val ponger: Future[ActorRef[Ping.type]] = system.ask(SpawnProtocol.Spawn(pongerBehavior, "ponger", Props.empty, _))
  for (pongerRef <- ponger) pongerRef ! Ping
