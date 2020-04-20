name := "random-util"

inThisBuild(
  List(
    organization := "",
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
    )
  )
)

version := "0.1"

scalaVersion := "2.13.1"

val zioVersion = "1.0.0-RC18-2"

libraryDependencies ++= Seq(
  "dev.zio" %% "zio" % zioVersion,
  "dev.zio" %% "zio-test" % zioVersion % "test",
  "dev.zio" %% "zio-test-sbt" % zioVersion % "test"
)
testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
