import sbt._
import Keys._
import PlayProject._

object ApplicationBuild extends Build {
    val core = Project("core", file("."))
    val web = PlayProject("web", path = file("web"), mainLang = SCALA) dependsOn(core)
}
