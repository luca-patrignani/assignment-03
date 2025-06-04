package com.example

import java.lang.Math.clamp

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

  //Distanza toroidale
  def tDistance(other: Vector2d)(using space: Vector2d): Double =
    val dx = math.abs(x - other.x)
    val dy = math.abs(y - other.y)
    val wrappedDx = math.min(dx, space.x - dx)
    val wrappedDy = math.min(dy, space.y - dy)
    math.hypot(wrappedDx, wrappedDy)

  /** Vettore differenza toroidale: direzione più corta in uno spazio che si avvolge */
  def --(other: Vector2d)(using space: Vector2d ): Vector2d =
    val dx = minimalOffset(x, other.x, space.x)
    val dy = minimalOffset(y, other.y, space.y)
    Vector2d(dx, dy)

  private def minimalOffset(a: Double, b: Double, max: Double): Double =
    val raw = b - a
    val wrapped = if raw > 0 then raw - max else raw + max
    if math.abs(raw) < math.abs(wrapped) then raw else wrapped

  /** Wrappa la posizione entro [0, width] e [0, height] */
  def wrapped(using space: Vector2d): Vector2d =
    Vector2d(
      (x % space.x + space.x) % space.x,
      (y % space.y + space.y) % space.y
    )
}

object Vector2d {
  val zero: Vector2d = Vector2d(0, 0)
}

case class Boid(position: Vector2d, velocity: Vector2d = Vector2d.zero) {
}

case class BoidRules(avoidRadius: Double, perceptionRadius: Double, maxSpeed: Double, tSpace: Vector2d) {
  given space: Vector2d = tSpace
  def separation(boidPosition: Vector2d, nearbyBoidsPositions: Seq[Vector2d]): Vector2d =
    nearbyBoidsPositions
      .filter(boidPosition.tDistance(_) < avoidRadius)
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

  def nearbyBoids(boid: Boid, allBoids: Seq[Boid]): Seq[Boid] =
    allBoids
      .filter(_ != boid)
      .filter(_.position.tDistance(boid.position) < perceptionRadius)

  def update(boid: Boid)(allBoids: Seq[Boid]): Boid =

    val nearby = nearbyBoids(boid, allBoids)

    val separationForce = separation(boid.position, nearby.map(_.position))
    val alignmentForce = alignment(boid.velocity, nearby.map(_.velocity))
    val cohesionForce = cohesion(boid.position, nearby.map(_.position))
    var newVelocity = boid.velocity + separationForce + alignmentForce + cohesionForce
    if newVelocity.magnitude > maxSpeed then
      newVelocity = newVelocity.normalized * maxSpeed
    val newPosition = boid.position + newVelocity
    Boid(newPosition.wrapped, newVelocity)
}

/*object Boid {
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
*/