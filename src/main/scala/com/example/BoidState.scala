package com.example

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.receptionist.Receptionist
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import com.example.Boid.Command

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

  // Toroidal Distance
  def tDistance(other: Vector2d)(using space: Vector2d): Double =
    val dx = math.abs(x - other.x)
    val dy = math.abs(y - other.y)
    val wrappedDx = math.min(dx, space.x - dx)
    val wrappedDy = math.min(dy, space.y - dy)
    math.hypot(wrappedDx, wrappedDy)

  /** Toroidal difference: (shortest distance in the wrapped space) */
  def --(other: Vector2d)(using space: Vector2d): Vector2d =
    val dx = minimalOffset(x, other.x, space.x)
    val dy = minimalOffset(y, other.y, space.y)
    Vector2d(dx, dy)

  private def minimalOffset(a: Double, b: Double, max: Double): Double =
    val raw = b - a
    val wrapped = if raw > 0 then raw - max else raw + max
    if math.abs(raw) < math.abs(wrapped) then raw else wrapped

  /** Wrappa pos into [0, width] and [0, height] */
  def wrapped(using space: Vector2d): Vector2d =
    Vector2d(
      (x % space.x + space.x) % space.x,
      (y % space.y + space.y) % space.y
    )
}

object Vector2d {
  val zero: Vector2d = Vector2d(0, 0)
}

case class BoidState(position: Vector2d, velocity: Vector2d = Vector2d.zero) extends Command

case class BoidRules(avoidRadius: Double, perceptionRadius: Double, maxSpeed: Double, tSpace: Vector2d) {
  given space: Vector2d = tSpace
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
    BoidState(newPosition.wrapped, newVelocity)
}

object Boid {
  sealed trait Command extends Message

  case object Stop extends Command

  case class UpdateWeights(separation: Double, cohesion: Double, alignment: Double) extends Command

  case class RequestInfo(replyTo: ActorRef[Command]) extends Command

  case object Resume extends Command

  def apply(
      state: BoidState,
      separationWeight: Double,
      alignmentWeight: Double,
      cohesionWeight: Double,
      period: FiniteDuration,
      frontends: List[ActorRef[BoidsRender.Render]] = List.empty
  )(using random: Random): Behavior[Command | Receptionist.Listing] =
    Behaviors.setup[Command | Receptionist.Listing] { ctx =>
      ctx.system.receptionist ! Receptionist.Subscribe(BoidsRender.Service, ctx.self)
      Behaviors.withTimers { timers =>
        timers.startTimerAtFixedRate(state, period)
        boidLogic(state, separationWeight, alignmentWeight, cohesionWeight, ctx, frontends)
      }
    }

  private def boidLogic(
      state: BoidState,
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
        boidLogic(state, separationWeight, alignmentWeight, cohesionWeight, ctx, services)

    case UpdateWeights(separation, alignment, cohesion) =>
      boidLogic(state, separation, alignment, cohesion, ctx, frontends)

    case RequestInfo(replyTo) => ???
    // replyTo ! ctx.

    case Stop => Behaviors.stopped

    // case Resume => ???
  }
}
