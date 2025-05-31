package com.example

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
}

object Vector2d {
  val zero: Vector2d = Vector2d(0, 0)
}

case class Boid(position: Vector2d, velocity: Vector2d = Vector2d.zero) {
}

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

  def nearbyBoids(boid: Boid, allBoids: Seq[Boid]): Seq[Boid] =
    allBoids
      .filter(_ != boid)
      .filter(_.position.distance(boid.position) < perceptionRadius)

  def update(boid: Boid)(allBoids: Seq[Boid]): Boid =
    val nearby = nearbyBoids(boid, allBoids)

    val separationForce = separation(boid.position, nearby.map(_.position))
    val alignmentForce = alignment(boid.velocity, nearby.map(_.velocity))
    val cohesionForce = cohesion(boid.position, nearby.map(_.position))

    var newVelocity = boid.velocity + separationForce + alignmentForce + cohesionForce
    if newVelocity.magnitude > maxSpeed then
      newVelocity = newVelocity.normalized * maxSpeed

    val newPosition = boid.position + newVelocity
    Boid(newPosition, newVelocity)
}
