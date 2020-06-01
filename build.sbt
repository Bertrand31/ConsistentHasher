name := "ConsistentHasher"

version := "1"

maintainer := "bertrandjun@gmail.com"

scalaVersion := "2.13.2"

libraryDependencies ++= Seq(
  // Tests
  "org.scalatest" %% "scalatest" % "3.1.2",
  // Misc
  "org.typelevel" %% "cats-core" % "2.1.1",
)

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.10.3")

scalacOptions ++= Seq(
  "-deprecation", // Warn about deprecated features
  "-encoding", "UTF-8", // Specify character encoding used by source files
  "-feature", // Emit warning and location for usages of features that should be imported explicitly
  "-language:existentials", // Existential types (besides wildcard types) can be written and inferred
  "-language:higherKinds", // Allow higher-kinded types
  "-unchecked", // Enable additional warnings where generated code depends on assumptions
  "-Xlint:_", // Enable all available style warnings
  "-Ywarn-macros:after", // Only inspect expanded trees when generating unused symbol warnings
  "-Ywarn-unused:_", // Enables all unused warnings
  "-Ywarn-value-discard", // Warn when non-Unit expression results are unused
)

scalacOptions in Test --= Seq(
  "-Xlint:_",
  "-Ywarn-unused-import",
)

javaOptions ++= Seq(
  "-XX:+CMSClassUnloadingEnabled", // Enable class unloading under the CMS GC
  "-Xms2g",
  "-Xmx12g",
  // "-XX:+UseParNewGC",
)

enablePlugins(JavaServerAppPackaging)
