package com.example

import akka.actor.typed.scaladsl.*
import akka.actor.typed.{ActorSystem, Behavior}
import akka.cluster.typed.Cluster
import com.example.Boids.Boid
import com.example.BoidsRender.*
import com.typesafe.config.ConfigFactory

import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.language.postfixOps
import scala.util.Random
// create ants and then an frontend to visualise them.
// It uses role to decide what kind of ActorSystem the node should deploy
object Root:
  def apply(boids: Int = 100, period: FiniteDuration = 60 milliseconds): Behavior[Nothing] = Behaviors.setup { ctx =>
    val cluster = Cluster(ctx.system)
    if (cluster.selfMember.hasRole(Roles.frontend)) BoidsRender().narrow
    else
      (0 to boids) foreach (i => {
        given random: Random = Random()
        val pos = Vector2d(random.nextInt(width), random.nextInt(height))
        ctx.spawnAnonymous(Boid.apply(pos,Vector2d.zero,1,1,1, period))
      })
      Behaviors.empty
  //Behaviors.empty
  }

@main def multipleBoids: Unit =
  startupWithRole(Roles.frontend, seeds.head)(Root())
  startupWithRole(Roles.backend, seeds.last)(Root())

@main def anotherFrontend: Unit =
  startupWithRole(Roles.frontend, 8081)(Root())

@main def anotherBackend: Unit =
  startupWithRole(Roles.backend, 8082)(Root())
