import sbt._
import Keys._

import com.typesafe.sbt.packager.Keys._
import com.typesafe.sbt.SbtNativePackager._

// Packaging for basic java application 
object Packaging {
  def settings: Seq[Setting[_]] =
    packagerSettings ++ 
    libSettings
    
  def libSettings: Seq[Setting[_]] = Seq(
    mappings in Universal <++= (Keys.managedClasspath in Compile) map depMappings,
    mappings in Universal <+= (Keys.packageBin in Compile) map { jar =>
      jar -> ("lib/" + jar.getName)
    }
  )
  // Converts a managed classpath into a set of lib mappings.
  def depMappings(deps: Seq[Attributed[File]]): Seq[(File,String)] = 
    for {
      dep <- deps
      file = dep.data
      // TODO - Figure out what to do with jar files.
      if file.isFile
    } yield dep.data -> ("lib/" + dep.data.getName)
}