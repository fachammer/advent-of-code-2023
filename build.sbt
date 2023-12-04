lazy val root = project
  .in(file("."))
  .settings(
    name         := "Advent of Code 2023",
    version      := "0.1.0-SNAPSHOT",
    scalaVersion := "3.3.1",
    testOptions in Test ++= Seq(
      Tests.Argument("-oD"),
      Tests.Argument("-W", "10", "10")
    ),
    libraryDependencies ++= Seq(
      "com.lihaoyi"   %% "pprint"     % "0.7.0",
      "com.lihaoyi"   %% "sourcecode" % "0.3.1",
      "org.scalatest" %% "scalatest"  % "3.2.17" % "test"
    )
  )
