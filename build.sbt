name := "rule-engine"

version := "1.0"

scalaVersion := "2.12.10"

libraryDependencies ++= Seq(
  "com.typesafe" % "config" % "1.4.0",
  "com.github.pureconfig" %% "pureconfig" % "0.12.3",
  "org.scalatest" %% "scalatest" % "3.1.1" % "test",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"
)
