lazy val root = (project in file("."))
  .settings(
    organization := "com.overthefinishline",
    name := "dashboard-server",
    version := "1.0-SNAPSHOT",
    scalaVersion := "2.11.8"
  )

mainClass in Compile := Some("com.overthefinishline.dashboard.Application")

assemblyOutputPath in assembly := new File("target/dashboard-server.jar")
test in assembly := {}

val akkaVersion = "2.4.7"
val googleHttpVersion = "1.22.0"
libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-http-core" % akkaVersion,
  "com.typesafe.akka" %% "akka-http-experimental" % akkaVersion,
  "com.typesafe.akka" %% "akka-http-spray-json-experimental" % akkaVersion,
  "com.typesafe.akka" %% "akka-http-testkit" % akkaVersion % Test,

  "com.google.http-client" % "google-http-client-jackson" % googleHttpVersion,
  "com.google.oauth-client" % "google-oauth-client" % googleHttpVersion,

  "org.scalatest" %% "scalatest" % "3.0.0-RC1" % Test
)
