inThisBuild(
  List(
    organization := "com.github.BraianIDeRoo",
    homepage := Some(url("https://github.com/BraianIDeRoo/random-util")),
    licenses := List(
      "Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")
    ),
    developers := List(
      Developer(
        "BraianIDeRoo",
        "Braian De Roo",
        "braianideroo@gmail.com",
        url("https://github.com/BraianIDeRoo")
      )
    ),
    scmInfo := Some(
      ScmInfo(
        url("https://github.com/BraianIDeRoo/random-util"),
        "git@github.com:BraianIDeRoo/random-util.git"
      )
    )
  )
)
ThisBuild / scalaVersion := "2.13.2"

publishTo := Some(
  if (isSnapshot.value)
    Opts.resolver.sonatypeSnapshots
  else
    Opts.resolver.sonatypeStaging
)

val zioVersion = "1.0.0-RC19-2"

val randomUtil = crossProject(JSPlatform, JVMPlatform)
  .in(file("."))
  .settings(
    name := "random-util",
    version := "0.5.0",
    libraryDependencies ++= Seq(
      "dev.zio" %%% "zio" % zioVersion,
      "dev.zio" %%% "zio-test" % zioVersion % "test",
      "dev.zio" %%% "zio-test-sbt" % zioVersion % "test"
    ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
  )
  .jsSettings(
    scalaJSUseMainModuleInitializer := true,
    libraryDependencies += "io.github.cquiroz" %%% "scala-java-time" % "2.0.0"
  )

lazy val root = project
  .in(file("."))
  .aggregate(randomUtil.js, randomUtil.jvm)
  .settings(scalaVersion := "2.13.2", publish := {}, publishLocal := {})
