ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.8"

lazy val root = (project in file("."))
  .settings(
    name := "Log_Analyzer",
    libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.4",
    libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.10",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % Test,
    libraryDependencies += "org.apache.commons" % "commons-lang3" % "3.12.0"
  )
