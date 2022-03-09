import Dependencies._

ThisBuild / scalaVersion     := "2.13.1"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "io.github.surajgharat"
ThisBuild / organizationName := "surajgharat"
ThisBuild / organizationHomepage := Some(url("https://github.com/iamsurajgharat"))

lazy val root = (project in file("."))
  .settings(
    name := "Expression Tree",
    libraryDependencies += scalaTest % Test
  )

// ThisBuild / scmInfo := Some(
//   ScmInfo(
//     url("https://github.com/iamsurajgharat/expression-tree"),
//     "scm:git@github.com:iamsurajgharat/expression-tree.git"
//   )
// )
// ThisBuild / developers := List(
//   Developer(
//     id    = "iamsurajgharat",
//     name  = "Suraj Gharat",
//     email = "mr.surajgharat2@gmail.com",
//     url   = url("https://github.com/iamsurajgharat")
//   )
// )

// ThisBuild / description := "A scala library to convert predicates or expressions into runtime lambdas"
// ThisBuild / licenses := List("Apache 2" -> new URL("http://www.apache.org/licenses/LICENSE-2.0.txt"))
// ThisBuild / homepage := Some(url("https://github.com/example/project"))

// // Remove all additional repository other than Maven Central from POM
// ThisBuild / pomIncludeRepository := { _ => false }
// ThisBuild / publishTo := {
//   val nexus = "https://oss.sonatype.org/"
//   if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
//   else Some("releases" at nexus + "service/local/staging/deploy/maven2")
// }
// ThisBuild / publishMavenStyle := true
