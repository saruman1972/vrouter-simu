name := "vrouter-simu"

version := "0.2"

scalaVersion := "2.10.7"

scalacOptions ++= Seq("-deprecation")

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

resolvers += "scala sbt Repository" at "http://dl.bintray.com/sbt/sbt-plugin-releases/"

libraryDependencies += "com.typesafe.akka" % "akka-actor" % "2.0.1"

libraryDependencies <+= scalaVersion { sv =>
  "org.scala-lang" % "scala-swing" % sv
}

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "1.9.1" % "test",
  "junit" % "junit" % "4.11" % "test"
)

libraryDependencies += "org.swinglabs" % "swingx" % "1.6" % "compile" withSources()

libraryDependencies += "com.github.benhutchison" % "scalaswingcontrib" % "1.5"

libraryDependencies += "commons-codec" % "commons-codec" % "1.7"

retrieveManaged := true

initialCommands in console := """
  import scala.swing._
  import test._
  import Swing._
"""

seq(com.github.retronym.SbtOneJar.oneJarSettings: _*)

libraryDependencies += "commons-lang" % "commons-lang" % "2.6"

mainClass in oneJar := Some("com.allinfinance.vrouter.simu.VRouterSimu")

// unmanagedResourceDirectories in Compile <+= baseDirectory( _ / "src" )
unmanagedResourceDirectories in Compile <<= Seq(scalaSource in Compile).join

