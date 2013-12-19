import sbt._
import sbt.Keys._

object ScalaRayTracerBuild extends Build {

  lazy val scalaRayTracer = Project(
    id = "scala-ray-tracer",
    base = file("."),
    settings = Project.defaultSettings ++ Seq(
      name := "Scala Ray Tracer",
      version := "0.1-SNAPSHOT",
      scalaVersion := "2.10.2",
      scalacOptions ++= Seq("-feature", "-language:postfixOps")
    )
  )
}
