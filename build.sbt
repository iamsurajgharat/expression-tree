import Dependencies._

ThisBuild / scalaVersion     := "2.13.1"
ThisBuild / version          := "0.0.1"
ThisBuild / organization     := "io.github.iamsurajgharat"
ThisBuild / organizationName := "iamsurajgharat"
ThisBuild / organizationHomepage := Some(url("https://github.com/iamsurajgharat"))

lazy val root = (project in file("."))
  .settings(
    name := "Expression Tree",
    libraryDependencies += scalaTest % Test
  )

libraryDependencies += "com.github.nscala-time" %% "nscala-time" % "2.30.0"

ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/iamsurajgharat/expression-tree"),
    "scm:git@github.com:iamsurajgharat/expression-tree.git"
  )
)
ThisBuild / developers := List(
  Developer(
    id    = "iamsurajgharat",
    name  = "Suraj Gharat",
    email = "mr.surajgharat2@gmail.com",
    url   = url("https://github.com/iamsurajgharat")
  )
)

ThisBuild / description := "A scala library to convert predicates or expressions into runtime lambdas"
ThisBuild / licenses := List("MIT License" -> new URL("https://opensource.org/licenses/MIT"))
ThisBuild / homepage := Some(url("https://github.com/iamsurajgharat/expression-tree"))

// Remove all additional repository other than Maven Central from POM
ThisBuild / pomIncludeRepository := { _ => false }
ThisBuild / publishTo := {
  val nexus = "https://s01.oss.sonatype.org/"
  if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
  else Some("releases" at nexus + "service/local/staging/deploy/maven2")
}
ThisBuild / publishMavenStyle := true
