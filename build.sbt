
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



