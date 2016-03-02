organization := "edu.berkeley.eecs"

version := "0.1"

scalaVersion in ThisBuild := "2.11.7"

val prjSettings = Project.defaultSettings ++ Seq(
  scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-language:reflectiveCalls",
                        "-language:implicitConversions", "-language:existentials"),
  libraryDependencies += "org.json4s" %% "json4s-native" % "3.3.0",
  libraryDependencies  ++= Seq(
    "org.scalanlp" %% "breeze" % "0.12",
    "org.scalanlp" %% "breeze-natives" % "0.12",
    "org.scalanlp" %% "breeze-viz" % "0.12"
  ),
  libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-compiler" % _ ) 
)

lazy val ChiselDSP = ProjectRef(
  uri("git://github.com/ucb-art/ChiselDSP.git"),
  "chisel-dsp"
)

lazy val root = Project(
  id = "fft",
  base = file("."),
  settings = prjSettings
).dependsOn(ChiselDSP
)
