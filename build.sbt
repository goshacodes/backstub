
lazy val backstub = (project in file("."))
  .settings(
    name := "backstub",
    scalaVersion := "3.4.3",
    scalacOptions ++=
      Seq(
        "-experimental",
        //"-Xcheck-macros",
        //"-explain"
      ),
    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % "1.0.0" % Test
    )
  )


