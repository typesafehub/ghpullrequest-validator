

name := "ghpullreq-validator"

organization := "com.typesafe"


libraryDependencies ++= Seq(
  "net.databinder" %% "dispatch-http-json" % "0.8.8",
  "net.databinder" %% "dispatch-http" % "0.8.8",
  "net.liftweb" %% "lift-json" % "2.4-M5"
  )
