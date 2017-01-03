name := "lehel"

version := "1.0"

scalaVersion := "2.11.8"

addCompilerPlugin("com.milessabin" % "si2712fix-plugin" % "1.0.1" cross CrossVersion.full) //without plugin compile will fail

// scalaOrganization in ThisBuild := "org.typelevel"

libraryDependencies ++= Seq(
  "jline" % "jline" % "2.12",
  "org.typelevel" %% "cats" % "0.8.0",
  "com.github.pathikrit" %% "better-files" % "2.16.0",
  "org.scala-lang" % "scala-compiler" % "2.11.8"
)

// scalacOptions += "-Ypartial-unification" // enable fix for SI-2712

// scalacOptions += "-Yliteral-types"       // enable SIP-23 implementation
