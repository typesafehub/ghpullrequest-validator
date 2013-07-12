import AssemblyKeys._

assemblySettings

scalaVersion := "2.10.2"

resolvers += "Typesafe Releases" at "http://repo.typesafe.com/typesafe/releases"

name := "ghpr"

version := "0.2-SNAPSHOT"

organization := "com.typesafe"


libraryDependencies ++= Seq(
  "net.databinder"    %% "dispatch-http-json" % "0.8.9",
  "net.databinder"    %% "dispatch-http"      % "0.8.9",
  "net.liftweb"       %% "lift-json"          % "2.5",
  "com.typesafe.akka" %% "akka-actor"         % "2.2.0",
  "com.typesafe"      % "config"              % "1.0.0"
)
