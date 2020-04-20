import Dependencies._

scalaVersion := "2.13.1"

lazy val root = (project in file("."))
  .settings(
    name := "functional-programming-in-scala",
    libraryDependencies += scalaTest % Test
  )
