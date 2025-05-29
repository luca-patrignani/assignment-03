package com.example

import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Assertions._

object BoidSeparationTest {
  private val testBoid = Vector2d(100, 100)

  @Test
  def testSeparation(): Unit =
    val nearbyBoids = Seq(
      Vector2d(90, 100),   // 10 units to the left
      Vector2d(110, 100),  // 10 units to the right
      Vector2d(100, 90)    // 10 units above
    )
    val separationForce = Boids.separation(testBoid, nearbyBoids)
    assertTrue(separationForce.y > 0)
    assertTrue(Math.abs(separationForce.x) < 0.1)

  @Test
  def testSeparationWithDistantBoid(): Unit =
    val farBoid = Vector2d(200, 200)  // Far away
    val separationForce = Boids.separation(testBoid, Seq(farBoid))
    assertEquals(Vector2d.zero, separationForce)
}

object BoidAlignmentTest {
  private val testVelocity = Vector2d(1, 0)  // Moving right

  @Test
  def testBasicAlignment(): Unit =
    val nearbyVelocities = Seq(
      Vector2d(0, 1),   // Moving up
      Vector2d(0, 1),   // Moving up
      Vector2d(0, 1)    // Moving up
    )
    val alignmentForce = Boids.alignment(testVelocity, nearbyVelocities)
    // Should steer upward to match neighbors
    assertTrue(alignmentForce.y > 0)
    assertTrue(alignmentForce.x < 0)

  @Test
  def testAlignmentWithNoNeighbors(): Unit =
    val alignmentForce = Boids.alignment(testVelocity, Seq.empty)
    assertEquals(Vector2d.zero, alignmentForce)

  @Test
  def testAlignmentWithSameVelocity(): Unit =
    val nearbyVelocities = Seq(
      Vector2d(1, 0),
      Vector2d(1, 0)
    )
    val alignmentForce = Boids.alignment(testVelocity, nearbyVelocities)
    assertEquals(Vector2d.zero, alignmentForce)
}
