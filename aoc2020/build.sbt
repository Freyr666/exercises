import Dependencies._

ThisBuild / scalaVersion     := "2.13.3"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "org.meadofpoetry"

lazy val root = (project in file("."))
  .settings(
    name := "aoc2020",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.1.1",
      "org.typelevel" %% "cats-effect" % "2.3.0",
      scalaTest % Test
    )
  )
