import scala.sys.process._
// OBS: sbt._ has also process. Importing scala.sys.process 
// and explicitly using it ensures the correct operation

organization := "Chisel-blocks"

name := "fir"

version := scala.sys.process.Process("git rev-parse --short HEAD").!!.mkString.replaceAll("\\s", "")

scalaVersion := "2.12.3"

// [TODO] what are these needed for? remove if obsolete
def scalacOptionsVersion(scalaVersion: String): Seq[String] = {
  Seq() ++ {
    // If we're building with Scala > 2.11, enable the compile option
    //  switch to support our anonymous Bundle definitions:
    //  https://github.com/scala/bug/issues/10047
    CrossVersion.partialVersion(scalaVersion) match {
      case Some((2, scalaMajor: Int)) if scalaMajor < 12 => Seq()
      case _ => Seq("-Xsource:2.11")
    }
  }
}

def javacOptionsVersion(scalaVersion: String): Seq[String] = {
  Seq() ++ {
    // Scala 2.12 requires Java 8. We continue to generate
    //  Java 7 compatible code for Scala 2.11
    //  for compatibility with old clients.
    CrossVersion.partialVersion(scalaVersion) match {
      case Some((2, scalaMajor: Int)) if scalaMajor < 12 =>
        Seq("-source", "1.7", "-target", "1.7")
      case _ =>
        Seq("-source", "1.8", "-target", "1.8")
    }
  }
}

// Parse the version of a submodle from the git submodule status
// for those modules not version controlled by Maven or equivalent
def gitSubmoduleHashSnapshotVersion(submod: String): String = {
    val shellcommand =  "git submodule status | grep -w %s | awk '{print substr($1,0,7)}'".format(submod)
    scala.sys.process.Process(Seq("/bin/sh", "-c", shellcommand )).!!.mkString.replaceAll("\\s", "")
}


// [TODO] what are these needed for? remove if obsolete
crossScalaVersions := Seq("2.11.11", "2.12.3")
scalacOptions ++= scalacOptionsVersion(scalaVersion.value)
javacOptions ++= javacOptionsVersion(scalaVersion.value)

// [TODO] what are these needed for? remove if obsolete
resolvers ++= Seq(
  Resolver.sonatypeRepo("snapshots"),
  Resolver.sonatypeRepo("releases")
)
// [TODO]: Is this redundant?
resolvers += "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"

// Provide a managed dependency on X if -DXVersion="" is supplied on the command line.
// [TODO] is simpler clearer?
val defaultVersions = Map(
  "chisel3" -> "3.1.7",
  "chisel-iotesters" -> "1.2.9",
  "dsptools" -> "1.1.8"
  )
libraryDependencies ++= (Seq("chisel3","dsptools", "chisel-iotesters").map {
  dep: String => "edu.berkeley.cs" %% dep % sys.props.getOrElse(dep + "Version", defaultVersions(dep)) })


libraryDependencies  ++= Seq(
//  // Last stable release
  "org.scalanlp" %% "breeze" % "0.13.2",
  
// Native libraries are not included by default. add this if you want them (as of 0.7)
  // Native libraries greatly improve performance, but increase jar sizes. 
  // It also packages various blas implementations, which have licenses that may or may not
  // be compatible with the Apache License. No GPL code, as best I know.
  "org.scalanlp" %% "breeze-natives" % "0.13.2",
  
  // The visualization library is distributed separately as well.
  // It depends on LGPL code
  "org.scalanlp" %% "breeze-viz" % "0.13.2"
)

