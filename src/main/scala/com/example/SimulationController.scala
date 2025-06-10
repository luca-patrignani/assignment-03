package com.example

import akka.actor.typed.receptionist.Receptionist
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import akka.actor.typed.scaladsl.Behaviors

import scala.concurrent.duration.FiniteDuration
import scala.util.Random
import com.example.Boid
import com.example.BoidsRender // supponendo che AntsRender sia nell’altro file

private val FRAMERATE = 60
object SimulationController:

  sealed trait Command extends Message
  case class GenerateBoids(count: Int) extends Command

  def apply(period: FiniteDuration): Behavior[Command | Receptionist.Listing] =
    Behaviors.setup[Command | Receptionist.Listing] { ctx =>
      ctx.spawn(BoidsRender(ctx.self, FRAMERATE), "renderer")
      controllerLogic()

    }

  private def controllerLogic(): Behavior[Command | Receptionist.Listing] = {
    Behaviors.receiveMessage { 
      case GenerateBoids(count) =>
        // Genera `count` attori Ant con posizioni casuali
        val width = BoidsRender.width
        val height = BoidsRender.height

        given random: Random = Random()

        (0 until count).foreach { _ =>
          val pos = Vector2d(random.nextInt(BoidsRender.width), random.nextInt(BoidsRender.height))
          val vel = Vector2d(random.nextInt(3), random.nextInt(3))
          ctx.spawnAnonymous(Boid(BoidState(pos, vel), period))
        }
        controllerLogic()
          
      //case ??? => ???
          
      }
      Behaviors.same
    }
  }
  
  @main def runControllerApp(): Unit =
    // Creo il sistema tipizzato con protocollo Command
    ActorSystem(SimulationController(period = 60.millis), "AntsSystem")
