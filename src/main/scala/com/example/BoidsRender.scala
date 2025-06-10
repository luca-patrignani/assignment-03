package com.example

import akka.actor.typed.receptionist.{Receptionist, ServiceKey}
import akka.actor.typed.scaladsl.*
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import com.example.Boid.BoidCommand
import com.example.BoidsRender.RenderMessage.{Flush, MyListing, NewBoid}

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
    case NewBoid(boid: Behavior[Boid.BoidCommand])

  val Service: ServiceKey[RenderMessage] = ServiceKey[RenderMessage]("RenderService")
  val frameRate: Double = 60
  def apply(): Behavior[RenderMessage] =
    Behaviors.setup { ctx =>
      val frontendGui = BoidsSimulationGUI(generateBoids(ctx, _, _, _, _)) // init the gui
      frontendGui.startup(Array.empty)
      Behaviors.withTimers { timers =>
        timers.startTimerAtFixedRate(Flush, ((1 / frameRate) * 1000).toInt milliseconds)
        var toRender = Seq.empty[Vector2d]
        ctx.system.receptionist ! Receptionist.Register(Service, ctx.self)
        val listingResponseAdapter = ctx.messageAdapter[Receptionist.Listing](RenderMessage.MyListing.apply)
        var boids: Seq[ActorRef[BoidCommand]] = Seq.empty
        Behaviors.receiveMessage {
          case Flush =>
            boids.foreach(_ ! Boid.RequestInfo(ctx.self))
            Behaviors.same

          case RenderMessage.RenderBoid(boidState) =>
            toRender = toRender :+ boidState.position
            frontendGui.render(toRender)
            Behaviors.same

          case NewBoid(boid) =>
            ctx.log.info("New boid created")
            val boidActor = ctx.spawnAnonymous(boid)
            boids = boids :+ boidActor
            Behaviors.same
        }
      }
    }

  private def generateBoids(ctx: ActorContext[RenderMessage], count: Int, s: Double, a: Double, c: Double): Unit = {
    (1 to count).foreach { _ =>
      val position = Vector2d(scala.util.Random.nextDouble() * width, scala.util.Random.nextDouble() * height)
      val velocity = Vector2d(scala.util.Random.nextDouble() * 2 - 1, scala.util.Random.nextDouble() * 2 - 1)
      ctx.pipeToSelf(Future.successful(NewBoid(Boid(BoidState(position, velocity), s, a, c)))) {
        case scala.util.Success(value) => value
      }
    }
  }

  @main def runBoidsRender(): Unit = {
    val system: ActorSystem[RenderMessage] = ActorSystem(BoidsRender(), "BoidsRenderSystem")
    system.receptionist ! Receptionist.Register(Service, system.ignoreRef)
  }
