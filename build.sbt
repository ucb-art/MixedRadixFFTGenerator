organization := "edu.berkeley.eecs"

version := "0.1"

scalaVersion in ThisBuild := "2.11.7"

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
  libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-compiler" % _ ) 
)

// Local ChiselDSP (for experimentation)
val ChiselDSPLocal = file("../ChiselDSP")
// Official ChiselDSP version
val ChiselDSPRepo = uri("git://github.com/ucb-art/ChiselDSP.git#master")
lazy val ChiselDSP = {
  if (ChiselDSPLocal.exists) ProjectRef(ChiselDSPLocal, "chisel-dsp")
  else ProjectRef(ChiselDSPRepo, "chisel-dsp")
}

// ARBOR for Matlab interfaces + Type classes
val arborVersion = "13b02578d48034f35fbd4f555e1316890b518b5d"
lazy val arbor = ProjectRef(
  uri("https://github.com/ucb-art/arbor.git#%s".format(arborVersion)),
  "root"
)

// Generator
lazy val root = Project(
  id = "fft",
  base = file("."),
  settings = prjSettings
).dependsOn(ChiselDSP, arbor)

// Define default sbt run main class
// mainClass in (Compile, run) := Some("FFT.MainWithMatlab")
mainClass in (Compile, run) := Some("FFT.RocketInterface")
