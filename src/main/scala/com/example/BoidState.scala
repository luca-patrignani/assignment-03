package com.example

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.receptionist.Receptionist
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.scaladsl.AskPattern.Askable
import com.example.BoidsRender.RenderMessage

import scala.concurrent.duration.DurationInt
import java.lang.Math.clamp
import scala.concurrent.duration.FiniteDuration
import scala.util.Random

case class Vector2d(x: Double, y: Double) {

  def +(other: Vector2d): Vector2d = Vector2d(x + other.x, y + other.y)

  def -(other: Vector2d): Vector2d = Vector2d(x - other.x, y - other.y)

  def *(scalar: Double): Vector2d = Vector2d(x * scalar, y * scalar)

  def /(scalar: Double): Vector2d = this * (1.0 / scalar)

  def magnitude: Double = math.sqrt(x * x + y * y)

  def normalized: Vector2d =
    val mag = magnitude
    if (mag == 0) this else this * (1.0 / mag)

  def distance(other: Vector2d): Double =
    math.sqrt(math.pow(x - other.x, 2) + math.pow(y - other.y, 2))

  /** Wrappa pos into [0, width] and [0, height] */
  def wrapped(space: Vector2d): Vector2d =
    Vector2d(
      (x % space.x + space.x) % space.x,
      (y % space.y + space.y) % space.y
    )
}

object Vector2d {
  val zero: Vector2d = Vector2d(0, 0)
}

case class BoidState(position: Vector2d, velocity: Vector2d = Vector2d.zero) extends Boid.BoidCommand

case class BoidRules(avoidRadius: Double, perceptionRadius: Double, maxSpeed: Double) {
  def separation(boidPosition: Vector2d, nearbyBoidsPositions: Seq[Vector2d]): Vector2d =
    nearbyBoidsPositions
      .filter(boidPosition.distance(_) < avoidRadius)
      .map(otherBoidPosition => (boidPosition - otherBoidPosition).normalized)
      .foldLeft(Vector2d.zero)(_ + _)

  def alignment(boidVelocity: Vector2d, nearbyBoidsVelocities: Seq[Vector2d]): Vector2d = nearbyBoidsVelocities match
    case Seq() => Vector2d.zero
    case _ =>
      val averageVelocity = nearbyBoidsVelocities
        .foldLeft(Vector2d.zero)(_ + _)
        / nearbyBoidsVelocities.size
      (averageVelocity - boidVelocity).normalized

  def cohesion(boidPosition: Vector2d, nearbyBoidsPositions: Seq[Vector2d]): Vector2d = nearbyBoidsPositions match
    case Seq() => Vector2d.zero
    case _ =>
      val centerOfMass = nearbyBoidsPositions
        .foldLeft(Vector2d.zero)(_ + _)
        / nearbyBoidsPositions.size
      (centerOfMass - boidPosition).normalized

  def nearbyBoids(boid: BoidState, allBoids: Seq[BoidState]): Seq[BoidState] =
    allBoids
      .filter(_ != boid)
      .filter(_.position.distance(boid.position) < perceptionRadius)

  def update(boid: BoidState)(allBoids: Seq[BoidState]): BoidState =
    val nearby = nearbyBoids(boid, allBoids)
    val separationForce = separation(boid.position, nearby.map(_.position))
    val alignmentForce = alignment(boid.velocity, nearby.map(_.velocity))
    val cohesionForce = cohesion(boid.position, nearby.map(_.position))
    var newVelocity = boid.velocity + separationForce + alignmentForce + cohesionForce
    if newVelocity.magnitude > maxSpeed then newVelocity = newVelocity.normalized * maxSpeed
    val newPosition = boid.position + newVelocity
    BoidState(newPosition.wrapped(Vector2d(500, 500)), newVelocity)
}

object Boid {
  sealed trait BoidCommand

  case object Stop extends BoidCommand

  case class UpdateWeights(separation: Double, cohesion: Double, alignment: Double) extends BoidCommand
  
  case class UpdatePos() extends BoidCommand

  case class RequestInfo(replyTo: ActorRef[RenderMessage.RenderBoid]) extends BoidCommand

  case class MyListing(listing: Receptionist.Listing) extends BoidCommand

  case object Resume extends BoidCommand

  val Service: akka.actor.typed.receptionist.ServiceKey[BoidCommand] =
    akka.actor.typed.receptionist.ServiceKey[BoidCommand]("BoidService")

  val period: FiniteDuration = 60.millis

  def apply(
      state: BoidState,
      separationWeight: Double,
      alignmentWeight: Double,
      cohesionWeight: Double,
  ): Behavior[BoidCommand] =
    Behaviors.setup[BoidCommand] { ctx =>
      ctx.system.receptionist ! Receptionist.Register(Service, ctx.self)
      Behaviors.withTimers { timers =>
        timers.startTimerAtFixedRate(state, period)
        receivingBehaviour(
          ctx,
          state,
          separationWeight,
          alignmentWeight,
          cohesionWeight
        )
      }
    }

  def receivingBehaviour(
                          ctx: ActorContext[BoidCommand],
                          state: BoidState,
                          separationWeight: Double,
                          alignmentWeight: Double,
                          cohesionWeight: Double,
                        ): Behavior[BoidCommand] = {
    Behaviors.receiveMessage[BoidCommand] {
      case UpdatePos() =>
        ctx.log.info(s"Updating position for boid at ${state.position}")
        val rules = BoidRules(avoidRadius = 20, perceptionRadius = 100, maxSpeed = 5)
        val listingResponseAdapter = ctx.messageAdapter[Receptionist.Listing](MyListing.apply)
        val otherBoids = ctx.system.receptionist ! Receptionist.Find(Boid.Service, listingResponseAdapter)
        val updatedState = ???
        receivingBehaviour(
          ctx,
          updatedState,
          separationWeight,
          alignmentWeight,
          cohesionWeight
        )

      case MyListing(listing) =>
        val adapter = ctx.messageAdapter[RenderMessage.RenderBoid](_.boidState)
        listing.serviceInstances(Boid.Service).foreach(_ ! RequestInfo(adapter))
        Behaviors.same

      case BoidState(position, velocity) =>
        ctx.log.info(s"Boid at position $position with velocity $velocity")
        // val rules = BoidRules(avoidRadius = 20, perceptionRadius = 100, maxSpeed = 5)
        // val updatedState = rules.update(state)(ctx.system.receptionist ! Receptionist.Find(Boid.Service))
        // ctx.self ! updatedState
        Behaviors.same

      case Stop =>
        ctx.log.info(s"Stopping boid at ${state.position}")
        Behaviors.stopped

      case UpdateWeights(separation, alignment, cohesion) =>
        ctx.log.info(s"Updating weights: separation=$separation, alignment=$alignment, cohesion=$cohesion")
        receivingBehaviour(
          ctx,
          state,
          separationWeight = separation,
          alignmentWeight = alignment,
          cohesionWeight = cohesion
        )

      case RequestInfo(replyTo) =>
        replyTo ! RenderMessage.RenderBoid(state)
        Behaviors.same

      case Resume =>
        Behaviors.same
    }
  }


}
