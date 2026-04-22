ThisBuild / scalaVersion := Versions.scala
ThisBuild / organization := "puzzles"
inThisBuild(
  List(
    semanticdbEnabled := true,
    semanticdbVersion := scalafixSemanticdb.revision
  )
)

lazy val root = (project in file("."))
  .settings(
    name := "programming-puzzles",

    libraryDependencies += "org.scalameta" %% "munit" % Versions.testing.munit % Test,
    testFrameworks += new TestFramework("munit.Framework"),

    // Run tests in parallel across packages
    Test / parallelExecution := true,

    // Show full stack traces on test failure
    Test / testOptions += Tests.Argument(new TestFramework("munit.Framework"), "-v"),

    scalacOptions ++= Seq(
      "-deprecation",
      "-feature",
      "-unchecked",
      "-Yexplicit-nulls"
    )
  )

addCommandAlias(
  "format",
  Seq(
    "scalafixAll",
    "scalafmtAll",
    "Test / scalafmt"
  ).mkString(";", ";", "")
)

addCommandAlias(
  "formatCheck",
  Seq(
    "scalafixAll --check",
    "scalafmtCheckAll",
    "Test / scalafmtCheck",
  ).mkString(";", ";", "")
)

addCommandAlias(
  "validate",
  Seq(
    "formatCheck",
    "undeclaredCompileDependenciesTest",
    "coverage",
    "test",
    "coverageReport",
    "coverageAggregate"
  ).mkString(";", ";", "")
)
