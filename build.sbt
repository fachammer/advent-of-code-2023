enablePlugins(JmhPlugin)
addCommandAlias("bench", "Jmh/run")
lazy val profile = inputKey[Unit]("Run visual Vm")
profile := {
  (Jmh / run).partialInput(" -bm ss -r 0 -f 1 -i 1 -w 0 -wi 0 -tu us -prof jfr")
    .evaluated

  import scala.sys.process.*
  "visualvm" !
}
lazy val root = project.in(file(".")).settings(
  name         := "Advent of Code 2023",
  version      := "0.1.0-SNAPSHOT",
  scalaVersion := "3.3.1",
  scalacOptions ++= Seq(
    "-deprecation",
    "-feature",
    "-indent",
    "-new-syntax",
    "-Wunused:all",
    "-Wvalue-discard",
  ),
  fork                          := true,
  Global / cancelable           := true,
  Global / onChangedBuildSource := ReloadOnSourceChanges,
  Test / testOptions ++=
    Seq(Tests.Argument("-oD"), Tests.Argument("-W", "10", "10")),
  libraryDependencies ++= Seq(
    "com.lihaoyi"   %% "pprint"     % "0.7.0",
    "com.lihaoyi"   %% "sourcecode" % "0.3.1",
    "org.scalatest" %% "scalatest"  % "3.2.17" % "test",
    "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4",
    "org.scalanlp"           %% "breeze"                     % "2.1.0",
  ),
  Jmh / sourceDirectory     := (Test / sourceDirectory).value,
  Jmh / classDirectory      := (Test / classDirectory).value,
  Jmh / dependencyClasspath := (Test / dependencyClasspath).value,
  Jmh / compile             := (Jmh / compile).dependsOn(Test / compile).value,
  Jmh / run                 := (Jmh / run).dependsOn(Jmh / compile).evaluated,
)
