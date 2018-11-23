import sbt.CrossVersion

name := "cats"

version := "0.1"

scalaVersion := "2.12.7"


// For reference: https://github.com/scalamacros/paradise
addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.8")


libraryDependencies ++= Seq(
  "com.github.mpilquist"       %% "simulacrum"               % "0.14.0"
)

scalacOptions += "-feature"