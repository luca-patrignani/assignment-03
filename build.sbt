name := "pcd-lab-akka-actors"

version := "1.0"

scalaVersion := "3.3.3"

resolvers += "Akka library repository".at("https://repo.akka.io/maven")

lazy val akkaVersion = "2.10.5"
lazy val akkaGroup = "com.typesafe.akka"
libraryDependencies ++= Seq(
  akkaGroup %% "akka-actor-typed" % "2.10.5",
  akkaGroup %% "akka-actor" % akkaVersion,
  "ch.qos.logback" % "logback-classic" % "1.5.6",
  akkaGroup %% "akka-actor-testkit-typed" % akkaVersion % Test,
  akkaGroup%% "akka-cluster-typed" % akkaVersion,
  "org.scalatest" %% "scalatest" % "3.2.19" % Test,
  "org.scala-lang.modules" %% "scala-swing" % "3.0.0",
  "org.junit.jupiter" % "junit-jupiter-api" % "5.12.2" % Test,
  "org.junit.jupiter" % "junit-jupiter-engine" % "5.12.2" % Test
)
