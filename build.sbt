val scala3Version = "3.3.1"

lazy val root = project
  .in(file("."))
  .settings(
    name         := "Advent of Code 2023",
    version      := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "com.lihaoyi"   %% "utest"     % "0.8.2",
      "org.scalactic" %% "scalactic" % "3.2.17",
      "org.scalatest" %% "scalatest" % "3.2.17" % "test"
    )
  )
