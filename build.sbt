import sbt.CrossVersion

name := "cats"

version := "0.1"

scalaVersion := "2.12.7"


// For reference: https://github.com/scalamacros/paradise
addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.8")


libraryDependencies ++= Seq(
  "com.github.mpilquist"       %% "simulacrum"               % "0.15.0",
  "org.typelevel"              %% "cats-core"                % "1.6.0",
  "org.typelevel"              %% "cats-free"                % "1.6.0",
  "org.typelevel"              %% "cats-mtl-core"            % "0.4.0",
  "org.scalamacros"            %% "resetallattrs"            % "1.0.0",
  "org.specs2"                 %% "specs2-core"              % "4.5.1",
  "org.specs2"                 %% "specs2-scalacheck"        % "4.5.1",
  "org.scalacheck"             %% "scalacheck"               % "1.14.0",
  "io.monix"                   %% "monix"                    % "2.3.3"
)

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-Ypartial-unification",
  "-feature",
  "-language:_"
)