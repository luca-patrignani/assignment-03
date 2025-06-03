package com.example

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.receptionist.Receptionist
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import com.example.BoidsRender

import scala.concurrent.duration.FiniteDuration
import scala.util.Random

case class Vector2d(x: Double, y: Double) {
  def +(other: Vector2d): Vector2d = Vector2d(x + other.x, y + other.y)
  def -(other: Vector2d): Vector2d = Vector2d(x - other.x, y - other.y)
  def *(scalar: Double): Vector2d = Vector2d(x * scalar, y * scalar)
  def /(scalar: Double): Vector2d = this * (1.0 / scalar)
  def magnitude: Double = math.sqrt(x * x + y * y)
  def normalize: Vector2d = {
    val mag = magnitude
    if (mag == 0) this else this * (1.0 / mag)
  }
  def distance(other: Vector2d): Double =
    math.sqrt(math.pow(x - other.x, 2) + math.pow(y - other.y, 2))
}

object Vector2d {
  val zero: Vector2d = Vector2d(0, 0)
}

object Boids {
  val AVOID_RADIUS = 50

  def separation(boidPosition: Vector2d, nearbyBoidsPositions: Seq[Vector2d]): Vector2d =
    nearbyBoidsPositions
      .filter(boidPosition.distance(_) < AVOID_RADIUS)
      .map(otherBoidPosition => (boidPosition - otherBoidPosition).normalize)
      .foldLeft(Vector2d.zero)(_ + _)

  def alignment(boidVelocity: Vector2d, nearbyBoidsVelocities: Seq[Vector2d]): Vector2d = nearbyBoidsVelocities match {
    case Seq() => Vector2d.zero
    case _ =>
      val averageVelocity = nearbyBoidsVelocities.foldLeft(Vector2d.zero)(_ + _) / nearbyBoidsVelocities.size
      (averageVelocity - boidVelocity).normalize
  }

  def cohesion(boidPosition: Vector2d, nearbyBoidsPositions: Seq[Vector2d]): Vector2d = nearbyBoidsPositions match {
    case Seq() => Vector2d.zero
    case _ =>
      val centerOfMass = nearbyBoidsPositions.foldLeft(Vector2d.zero)(_ + _) / nearbyBoidsPositions.size
      (centerOfMass - boidPosition).normalize
  }

//  case class Boid(
//                   position: Vector2d,
//                   velocity: Vector2d = Vector2d.zero,
//                   separationWeight: Double,
//                   alignmentWeight: Double,
//                   cohesionWeight: Double
//                 )

  object Boid {
    sealed trait Command
    case object Stop extends Command
    private case object UpdateVel extends Command
    private case object UpdatePos extends Command

    private val boids = Seq(Vector2d(10, 4), Vector2d(8, 2), Vector2d(10, 0))

    def apply(
               position: Vector2d,
               velocity: Vector2d = Vector2d.zero,
               separationWeight: Double,
               alignmentWeight: Double,
               cohesionWeight: Double,
               period: FiniteDuration,
               frontends: List[ActorRef[BoidsRender.Render]] = List.empty
             )(using random: Random): Behavior[Command | Receptionist.Listing] =
      Behaviors.setup { ctx =>
        ctx.system.receptionist ! Receptionist.Subscribe(BoidsRender.Service, ctx.self)
        Behaviors.withTimers { timers =>
          timers.startTimerAtFixedRate(UpdatePos, period)
          boidLogic(position, velocity, separationWeight, alignmentWeight, cohesionWeight, ctx, frontends)
        }
      }

    private def boidLogic(
                           position: Vector2d,
                           velocity: Vector2d,
                           separationWeight: Double,
                           alignmentWeight: Double,
                           cohesionWeight: Double,
                           ctx: ActorContext[Command | Receptionist.Listing],
                           frontends: List[ActorRef[BoidsRender.Render]]
                         ): Behavior[Command | Receptionist.Listing] = Behaviors.receiveMessage {
      case msg: Receptionist.Listing =>
        val services = msg.serviceInstances(BoidsRender.Service).toList
        if (services == frontends)
          Behaviors.same
        else
          boidLogic(position, velocity, separationWeight, alignmentWeight, cohesionWeight, ctx, services)

      case UpdateVel =>
        val sep = separation(position, boids) * separationWeight
        val coh = cohesion(position, boids) * cohesionWeight
        val ali = alignment(velocity, boids) * alignmentWeight
        val newVel = velocity + sep + coh + ali
        boidLogic(position, newVel, separationWeight, alignmentWeight, cohesionWeight, ctx, frontends)

      case UpdatePos =>
        val newPos = position + velocity
        boidLogic(newPos, velocity, separationWeight, alignmentWeight, cohesionWeight, ctx, frontends)

      case Stop => Behaviors.stopped
    }
  }
}
