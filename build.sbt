organization := "edu.berkeley.eecs"

version := "0.1"

scalaVersion in ThisBuild := "2.11.7"

val defaultVersions = Map(
  "tapeout" -> "0.1-SNAPSHOT",
  "firrtl" -> "1.1-SNAPSHOT",
  "dsptools" -> "1.0",
  "chisel3" -> "3.1-SNAPSHOT",
  "chisel-iotesters" -> "1.2-SNAPSHOT")

val prjSettings = Project.defaultSettings ++ Seq(
  scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-language:reflectiveCalls",
                        "-language:implicitConversions", "-language:existentials"),
  // JSON Interpreter
  libraryDependencies += "org.json4s" %% "json4s-native" % "3.3.0",
  // Breeze numerical processing libraries
  libraryDependencies  ++= Seq(
    "org.scalanlp" %% "breeze" % "0.12",
    "org.scalanlp" %% "breeze-natives" % "0.12",
    "org.scalanlp" %% "breeze-viz" % "0.12"
  ),
  // WISP plotting libraries
  libraryDependencies += "com.quantifind" %% "wisp" % "0.0.4",
  libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-compiler" % _ ),

  // CHISEL3
  libraryDependencies ++= (Seq("chisel3","chisel-iotesters","dsptools","firrtl", "tapeout").map {
  dep: String => "edu.berkeley.cs" %% dep % sys.props.getOrElse(dep + "Version", defaultVersions(dep)) }),

  libraryDependencies += "org.spire-math" %% "spire" % "0.11.0",

  libraryDependencies += "org.scalanlp" %% "breeze" % "0.12",

  libraryDependencies += "co.theasi" %% "plotly" % "0.1",

  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "2.2.5",
    "org.scalacheck" %% "scalacheck" % "1.12.4")
)

lazy val ChiselDSP = ProjectRef(file("ChiselDSP"), "chisel-dsp")

// ARBOR for Matlab interfaces + Type classes
/*
val arborVersion = "13b02578d48034f35fbd4f555e1316890b518b5d"
lazy val arbor = ProjectRef(
  uri("ssh://git@github.com/ucb-art/arbor.git#%s".format(arborVersion)),
  "root"
)
*/

// Generator
lazy val fft = Project(
  id = "fft",
  base = file("."),
  settings = prjSettings
).dependsOn(ChiselDSP)

// Define default sbt run main class
// mainClass in (Compile, run) := Some("FFT.MainWithMatlab")
// mainClass in (Compile, run) := Some("FFT.RocketInterfaceWrapper")
// mainClass in (Compile, run) := Some("FFT.DebugFFT")
mainClass in (Compile, run) := Some("dspblocks.fft.PeelingScheduling")