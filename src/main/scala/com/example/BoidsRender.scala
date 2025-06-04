package com.example

import akka.actor.typed.receptionist.{Receptionist, ServiceKey}
import akka.actor.typed.scaladsl.*
import akka.actor.typed.scaladsl.adapter.*
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import it.unibo.pcd.akka.Message

import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.language.postfixOps

object BoidsRender:
  // Constants
  val width = 800
  val height = 600
  sealed trait Command
  final case class Render(x: Int, y: Int, id: ActorRef[_]) extends Message with Command
  private case object Flush extends Command // Private message (similar to a private method in OOP)
  val Service = ServiceKey[Render]("RenderService")
  def apply(frameRate: Double = 60): Behavior[Command] = {
    Behaviors.setup { ctx =>
      val frontendGui = BoidsSimulationGUI // init the gui
      Behaviors.withTimers { timers =>
        timers.startTimerAtFixedRate(Flush, ((1 / frameRate) * 1000).toInt milliseconds)
        var toRender: Map[ActorRef[_], (Int, Int)] = Map.empty
        ctx.system.receptionist ! Receptionist.Register(Service, ctx.self)
        Behaviors.receiveMessage {
          case Render(x, y, id) =>
            ctx.log.info(s"render.. $id")
            toRender = toRender + (id -> (x, y))
            frontendGui.render(toRender.values.toList)
            Behaviors.same

          case Flush =>
            frontendGui.render(toRender.values.toList)
            Behaviors.same
        }
      }
    }
  }
