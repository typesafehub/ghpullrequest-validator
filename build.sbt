import com.typesafe.startscript.StartScriptPlugin

seq(StartScriptPlugin.startScriptForClassesSettings: _*)

resolvers += "Typesafe Releases" at "http://repo.typesafe.com/typesafe/releases"

name := "ghpr"

organization := "com.typesafe"

libraryDependencies ++= Seq(
  "net.databinder" % "dispatch-http-json_2.9.1" % "0.8.8",
  "net.databinder" % "dispatch-http_2.9.1" % "0.8.8",
  "net.liftweb" % "lift-json_2.9.1" % "2.4-M5",
  "com.typesafe.akka" % "akka-actor" % "2.0",
  "com.typesafe" % "config" % "0.4.0"
)
