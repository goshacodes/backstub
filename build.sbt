lazy val modules = file("modules")

lazy val backstub = project
  .in(file("."))
  .settings(
    commonSettings,
    publishTo := None
  )
  .dependsOn(core, cats, zio)
  .aggregate(core, cats, zio)

lazy val commonSettings =
  Seq(
    scalaVersion := "3.4.3",
    scalacOptions ++=
      Seq(
        "-experimental"
        // "-Xcheck-macros",
        // "-explain"
      ),
    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % "1.0.0" % Test
    )
  )

lazy val core = project.in(modules / "core")
  .settings(
    name := "backstub",
    commonSettings
  )

lazy val zio = project.in(modules / "zio")
  .dependsOn(core % "compile->compile;compile->test")
  .settings(
    name := "backstub-zio",
    commonSettings,
    libraryDependencies ++= {
      val zioVersion = "2.1.9"
      Seq(
        "dev.zio" %% "zio" % zioVersion,
        "dev.zio" %% "zio-test" % zioVersion % Test,
        "dev.zio" %% "zio-test-sbt" % zioVersion % Test
      )
    },
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
  )

lazy val cats = project.in(modules / "cats")
  .dependsOn(core)
  .settings(
    name := "backstub-cats-effect",
    commonSettings,
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect" % "3.5.4",
      "org.typelevel" %% "munit-cats-effect" % "2.0.0" % Test
    )
  )

inThisBuild(
  Seq(
    organization := "io.github.goshacodes",
    homepage := Some(url("https://https://github.com/goshacodes/backstub")),
    licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
    developers := List(
      Developer(
        "goshacodes",
        "Georgii Alekseevich Kovalev",
        "goshacodes@gmail.com",
        url("https://github.com/goshacodes")
      )
    ),
    sonatypeCredentialHost := xerial.sbt.Sonatype.sonatypeCentralHost
  )
)

sonatypeRepository := "https://s01.oss.sonatype.org/service/local"
