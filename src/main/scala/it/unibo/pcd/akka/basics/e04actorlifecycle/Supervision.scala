package it.unibo.pcd.akka.basics.e04actorlifecycle

import akka.actor.typed.{ActorSystem, Behavior, SupervisorStrategy, Terminated}
import akka.actor.typed.scaladsl.Behaviors

object SupervisedActor:
  def apply(supervisorStrategy: SupervisorStrategy): Behavior[String] = Behaviors
    .supervise[String](actor(""))
    .onFailure[RuntimeException](supervisorStrategy)

  def actor(prefix: String): Behavior[String] = Behaviors.receive:
    case (ctx, "fail") => throw new RuntimeException("Just fail")
    case (ctx, "quit") =>
      ctx.log.info("Quitting")
      ctx.log.info(s"Bye!! $prefix")
      Behaviors.stopped
    case (ctx, s) =>
      ctx.log.info(s"Got ${prefix + s}")
      actor(prefix + s)

object SupervisionExampleRestart extends App:
  val system = ActorSystem[String](SupervisedActor(SupervisorStrategy.restart), "supervision")
  for (cmd <- List("foo", "bar", "fail", "!!!", "fail", "quit")) system ! cmd

object SupervisionExampleResume extends App:
  val system = ActorSystem[String](SupervisedActor(SupervisorStrategy.resume), "supervision")
  for (cmd <- List("foo", "bar", "fail", "!!!", "fail", "quit")) system ! cmd

object SupervisionExampleStop extends App:
  val system = ActorSystem[String](SupervisedActor(SupervisorStrategy.stop), "supervision")
  for (cmd <- List("foo", "bar", "fail", "!!!", "fail", "quit")) system ! cmd

// A simple watching example
object WatchingExample extends App:
  val system = ActorSystem[String](
    Behaviors.setup[String]: ctx =>
      val child = ctx.spawn(SupervisedActor(SupervisorStrategy.restart), "child")
      ctx.watch(child)
      Behaviors.receiveMessage:
        case "kill" =>
          ctx.log.info("Killing child")
          ctx.stop(child)
          Behaviors.same
        case "quit" =>
          ctx.log.info("Quitting")
          Behaviors.same
        case other =>
          ctx.log.info(other)
          Behaviors.stopped
    ,
    "watching"
  )

  system ! "kill"
  system ! "quit"
