import sbt._
import Keys._
import com.typesafe.sbt.packager.Keys._
import com.typesafe.sbt.SbtNativePackager._
import com.typesafe.sbt.packager.linux.LinuxPackageMapping

// Packaging for basic java application 
object Packaging {
  def settings: Seq[Setting[_]] =
    packagerSettings ++ 
    JavaAppPackaging.settings ++
    Seq(
      packageSummary in Linux := "github pull request validator",
      packageDescription in Linux := " validates github pull requests using jenkins",
      maintainer in Debian := "Josh Suereth <joshua.suereth@typesafe.com>"
    )
}


object JavaAppPackaging {
  
  def settings =
    defaultUniversalSettings ++
    defaultLinuxSettings
  
  
  /// Universal packaging defaults.
  def defaultUniversalSettings: Seq[Setting[_]] = Seq(
    mappings in Universal <++= (Keys.managedClasspath in Compile) map universalDepMappings,
    mappings in Universal <+= (Keys.packageBin in Compile) map { jar =>
      jar -> ("lib/" + jar.getName)
    },
    mappings in Universal <++= (Keys.mainClass in Compile, target in Universal, name in Universal) map makeUniversalBinScript 
  )
  
  def makeUniversalBinScript(mainClass: Option[String], tmpDir: File, name: String): Seq[(File, String)] = 
    for(mc <- mainClass.toSeq) yield {
      val scriptBits = JavaAppBashScript.generateScript(mc)
      val script = tmpDir / "tmp" / "bin" / name
      IO.write(script, scriptBits)
      script -> ("bin/" + name)
    }
  
  // Converts a managed classpath into a set of lib mappings.
  def universalDepMappings(deps: Seq[Attributed[File]]): Seq[(File,String)] = 
    for {
      dep <- deps
      file = dep.data
      // TODO - Figure out what to do with jar files.
      if file.isFile
    } yield dep.data -> ("lib/" + dep.data.getName)
    
    
  // Default linux settings are driven off of the universal settings.  
  def defaultLinuxSettings: Seq[Setting[_]] = Seq(
    linuxPackageMappings <+= (mappings in Universal, name in Linux) map filterLibs,
    linuxPackageMappings <++= (mainClass in Compile, name in Linux, target in Linux) map makeLinuxBinScrit
  )
  
  def filterLibs(mappings: Seq[(File, String)], name: String): LinuxPackageMapping = {
    val libs = for {
      (file, location) <- mappings
      if location startsWith "lib/"
    } yield file -> ("/usr/share/"+name+"/" + location)
    packageMapping(libs:_*)
  }
  
    
  def makeLinuxBinScrit(mainClass: Option[String], name: String, tmpDir: File): Seq[LinuxPackageMapping] =
    for(mc <- mainClass.toSeq) yield {
      val scriptBits = JavaAppBashScript.generateScript(
          mainClass = mc,
          libDir = "/usr/share/" + name + "/lib")
      val script = tmpDir / "tmp" / "bin" / name
      IO.write(script, scriptBits)
      val scriptMapping = script -> ("/usr/bin/" + name)
      
      packageMapping(scriptMapping).withPerms("0755")
    }
}