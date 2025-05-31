package com.example

import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Assertions._

object BoidRulesTest {

  private val rules = BoidRules(avoidRadius = 20, perceptionRadius = 100)

  object BoidSeparationTest {
    private val testBoid = Vector2d(100, 100)

    @Test
    def testSeparation(): Unit =
      val nearbyBoids = Seq(
        Vector2d(90, 100),   // 10 units to the left
        Vector2d(110, 100),  // 10 units to the right
        Vector2d(100, 90)    // 10 units above
      )
      val separationForce = rules.separation(testBoid, nearbyBoids)
      assertTrue(separationForce.y > 0)
      assertTrue(Math.abs(separationForce.x) < 0.1)

    @Test
    def testSeparationWithDistantBoid(): Unit =
      val farBoid = Vector2d(200, 200)  // Far away
      val separationForce = rules.separation(testBoid, Seq(farBoid))
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
      val alignmentForce = rules.alignment(testVelocity, nearbyVelocities)
      // Should steer upward to match neighbors
      assertTrue(alignmentForce.y > 0)
      assertTrue(alignmentForce.x < 0)

    @Test
    def testAlignmentWithNoNeighbors(): Unit =
      val alignmentForce = rules.alignment(testVelocity, Seq.empty)
      assertEquals(Vector2d.zero, alignmentForce)

    @Test
    def testAlignmentWithSameVelocity(): Unit =
      val nearbyVelocities = Seq(
        Vector2d(1, 0),
        Vector2d(1, 0)
      )
      val alignmentForce = rules.alignment(testVelocity, nearbyVelocities)
      assertEquals(Vector2d.zero, alignmentForce)
  }

  object BoidCohesionTest {
    private val testPosition = Vector2d(100, 100)

    @Test
    def testBasicCohesion(): Unit =
      val nearbyPositions = Seq(
        Vector2d(120, 120),  // Above and right
        Vector2d(120, 120),  // Above and right
        Vector2d(120, 120)   // Above and right
      )
      val cohesionForce = rules.cohesion(testPosition, nearbyPositions)
      // Should steer toward center of mass (up and right)
      assertTrue(cohesionForce.x > 0)
      assertTrue(cohesionForce.y > 0)

    @Test
    def testCohesionWithNoNeighbors(): Unit =
      val cohesionForce = rules.cohesion(testPosition, Seq.empty)
      assertEquals(Vector2d.zero, cohesionForce)

    @Test
    def testCohesionWithSamePosition(): Unit =
      val nearbyPositions = Seq(
        Vector2d(100, 100),
        Vector2d(100, 100)
      )
      val cohesionForce = rules.cohesion(testPosition, nearbyPositions)
      assertEquals(Vector2d.zero, cohesionForce)
  }

  object BoidPerceptionTest {
    private val centerBoid = Boid(Vector2d(100, 100))

    @Test
    def testCollectNearbyBoids(): Unit =
      val allBoids = Seq(
        centerBoid,
        Boid(Vector2d(150, 150)), // Inside radius
        Boid(Vector2d(300, 300)) // Outside radius
      )

      val nearbyBoids = rules.nearbyBoids(centerBoid, allBoids)

      assertEquals(1, nearbyBoids.size)
      assertEquals(Vector2d(150, 150), nearbyBoids.head.position)

    @Test
    def testNoBoidsTooClose(): Unit =
      val allBoids = Seq(
        centerBoid,
        Boid(Vector2d(300, 300)),
        Boid(Vector2d(400, 400))
      )

      val nearbyBoids = rules.nearbyBoids(centerBoid, allBoids)
      assertTrue(nearbyBoids.isEmpty)
  }

}

