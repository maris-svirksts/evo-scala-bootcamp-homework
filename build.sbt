name := "evo-scala-bootcamp-homework"

version := "0.1"

scalaVersion := "2.13.3"

val catsVersion = "2.2.0"
val catsScalacheckVersion = "0.2.0"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.2.0" % "test",
  "org.typelevel" %% "cats-core" % catsVersion,
  "io.chrisdavenport" %% "cats-scalacheck" % catsScalacheckVersion % Test
)
