ThisBuild / scalaVersion := "3.3.3"
ThisBuild / organization := "puzzles"

lazy val root = (project in file("."))
  .settings(
    name := "programming-puzzles",

    libraryDependencies += "org.scalameta" %% "munit" % "1.0.0" % Test,
    testFrameworks += new TestFramework("munit.Framework"),

    // Run tests in parallel across packages
    Test / parallelExecution := true,

    // Show full stack traces on test failure
    Test / testOptions += Tests.Argument(new TestFramework("munit.Framework"), "-v"),

    scalacOptions ++= Seq(
      "-deprecation",
      "-feature",
      "-unchecked",
      "-Wunused:all",
      "-Wshadow:all",
      "-Yexplicit-nulls"
    )
  )
