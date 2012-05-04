import com.typesafe.startscript.StartScriptPlugin

seq(StartScriptPlugin.startScriptForClassesSettings: _*)

resolvers += "Typesafe Releases" at "http://repo.typesafe.com/typesafe/releases"

name := "ghpullreq-validator"

organization := "com.typesafe"

libraryDependencies ++= Seq(
  "net.databinder" %% "dispatch-http-json" % "0.8.8",
  "net.databinder" %% "dispatch-http" % "0.8.8",
  "net.liftweb" %% "lift-json" % "2.4-M5",
  "com.typesafe.akka" % "akka-actor" % "2.0",
  "com.typesafe" % "config" % "0.4.0"
)
