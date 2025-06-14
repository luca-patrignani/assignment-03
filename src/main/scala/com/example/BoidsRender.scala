package com.example

import akka.actor.typed.receptionist.{Receptionist, ServiceKey}
import akka.actor.typed.scaladsl.*
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import com.example.Boid.{BoidCommand, Stop, UpdateWeights}
import com.example.BoidsRender.RenderMessage.{
  Flush,
  MyListing,
  NewBoid,
  StartSimulation,
  StopSimulation,
  UpdateParameter
}

import scala.concurrent.Future
import scala.concurrent.duration.DurationInt
import scala.language.postfixOps

object BoidsRender:
  // Constants
  val width = 800
  val height = 600

  enum RenderMessage:
    case Flush
    case RenderBoid(boidState: BoidState)
    case MyListing(listing: Receptionist.Listing)
    case NewBoid(boidRefs: Seq[ActorRef[Boid.BoidCommand]])
    case GenerateBoids(count: Int, s: Double, a: Double, c: Double)
    case UpdateParameter(s: Double, a: Double, c: Double)
    case StartSimulation
    case StopSimulation

  val Service: ServiceKey[RenderMessage] = ServiceKey[RenderMessage]("RenderService")
  val frameRate: Double = 30

  def apply(): Behavior[RenderMessage] =
    Behaviors.setup { ctx =>
      val frontendGui = BoidsSimulationGUI(ctx.self) // init the gui
      frontendGui.startup(Array.empty)

      Behaviors.withTimers { timers =>
        ctx.system.receptionist ! Receptionist.Register(Service, ctx.self)
        var toRender = Seq.empty[Vector2d]
        var boids: Seq[ActorRef[BoidCommand]] = Seq.empty

        timers.startTimerAtFixedRate(Flush, (1000 / frameRate).toInt milliseconds)

        val listingResponseAdapter = ctx.messageAdapter[Receptionist.Listing](RenderMessage.MyListing.apply)
        Behaviors.receiveMessage {
          case Flush =>
            boids.foreach(_ ! Boid.RequestInfo(ctx.self))
            Behaviors.same

          case RenderMessage.RenderBoid(boidState) =>
            toRender = toRender :+ boidState.position
            if toRender.size == boids.size then
              frontendGui.render(toRender)
              toRender = Seq.empty // reset after rendering
            Behaviors.same

          case RenderMessage.NewBoid(boidRefs) =>
            ctx.log.info("New boids created")
            boids.foreach(_ ! Stop)
            boids = boidRefs
            Behaviors.same

          case StartSimulation =>
            boids.foreach(_ ! Boid.ResumeSimulation)
            Behaviors.same

          case StopSimulation =>
            boids.foreach(_ ! Boid.StopSimulation)
            Behaviors.same

          case RenderMessage.GenerateBoids(count, s, a, c) =>
            generateBoids(ctx, count, s, a, c)
            Behaviors.same

          case UpdateParameter(s, a, c) =>
            boids.foreach(_ ! Boid.UpdateWeights(s, a, c))
            Behaviors.same
        }
      }
    }

  private def generateBoids(ctx: ActorContext[RenderMessage], count: Int, s: Double, a: Double, c: Double): Unit = {
    val newBoidsRefs = for (_ <- 1 to count) yield {
      val position = Vector2d(
        scala.util.Random.nextDouble() * width,
        scala.util.Random.nextDouble() * height
      )
      val velocity = Vector2d(
        scala.util.Random.nextDouble() * 2 - 1,
        scala.util.Random.nextDouble() * 2 - 1
      )
      ctx.spawnAnonymous(Boid(BoidState(position, velocity), s, a, c))
    }
    ctx.self ! NewBoid(newBoidsRefs)

  }

  @main def runBoidsRender(): Unit = {
    val system: ActorSystem[RenderMessage] = ActorSystem(BoidsRender(), "BoidsRenderSystem")
    system.receptionist ! Receptionist.Register(Service, system.ignoreRef)
  }
