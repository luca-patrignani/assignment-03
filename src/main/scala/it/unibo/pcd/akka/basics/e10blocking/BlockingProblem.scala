package it.unibo.pcd.akka.basics.e10blocking

import akka.actor.typed.{ActorSystem, Behavior, DispatcherSelector, Signal}
import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext, Behaviors}
import com.typesafe.config.{Config, ConfigFactory}

import scala.concurrent.Future
import scala.concurrent.ExecutionContext

object BlockingProblem:
  def apply(i: Int): Behavior[String] = Behaviors.receive:
    case (ctx, any) =>
      println(s"actors $i")
      Thread.sleep(5000)
      Behaviors.same

  def usingFuture(i: Int): Behavior[String] = Behaviors.receive:
    case (ctx, any) =>
      given ExecutionContext = ctx.executionContext
      Future { Thread.sleep(5000); println(s"done $i") }
      ctx.log.info(s"actors $i")
      Behaviors.same

  def usingDispatcher(i: Int, dispatcher: String): Behavior[String] = Behaviors.receive:
    case (ctx, any) =>
      given ExecutionContext = ctx.system.dispatchers.lookup(DispatcherSelector.fromConfig(dispatcher))
      Future:
        Thread.sleep(5000); println(s"done $i")
      ctx.log.info(s"actors $i")
      Behaviors.same

object Spawner:
  def apply(factory: Int => Behavior[String]): Behavior["spawn"] =
    var start = 0
    Behaviors.receive: (ctx, _) =>
      start to start + 50 foreach (i => ctx.spawnAnonymous(factory(i)) ! "")
      start = 50
      Behaviors.same
@main def problem: Unit =
  val spawner = ActorSystem.create(Spawner(BlockingProblem.usingFuture), "slow")
  spawner ! "spawn"
  Thread.sleep(1000)
  spawner ! "spawn"

@main def solution: Unit =
  val blockingScheduler =
    """
      |my-blocking-dispatcher {
      |  type = Dispatcher
      |  executor = "thread-pool-executor"
      |  thread-pool-executor {
      |    fixed-pool-size = 16
      |  }
      |  throughput = 1
      |}
      |""".stripMargin
  // make a Config with just your special setting
  val myConfig = ConfigFactory.parseString(blockingScheduler);
  // load the normal config stack (system props,
  // then application.conf, then reference.conf)
  val regularConfig = ConfigFactory.load();
  // override regular stack with myConfig
  val combined = myConfig.withFallback(regularConfig);
  val spawner =
    ActorSystem.create(Spawner(i => BlockingProblem.usingDispatcher(i, "my-blocking-dispatcher")), "slow", myConfig)
  spawner ! "spawn"
  Thread.sleep(500)
  spawner ! "spawn"
