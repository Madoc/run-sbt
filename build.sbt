organization := "com.github.madoc"

name := "run-sbt"

version := "0.1-SNAPSHOT"

scalaVersion := "2.12.2"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-encoding", "utf8")

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.1" % "test" withSources(),
  "org.scalacheck" %% "scalacheck" % "1.13.4" % "test" withSources()
)

javaOptions ++= Seq("-Xdebug", "-Xrunjdwp:transport=dt_socket,server=y,suspend=n,address=5005")

javaOptions in (Test, run) += "-Djava.awt.headless=true"

jacoco.settings
