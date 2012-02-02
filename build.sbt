name := "twp4scala"

version := "0.1"

scalaVersion := "2.9.1"

mainClass := Some("twp4scala.example.Hello")

scalacOptions ++= Seq(
    "-deprecation"
  , "-unchecked"
  //, "-Xlog-implicits"
)

libraryDependencies ++= Seq(
  "dnsjava" % "dnsjava"% "2.1.1",
  "org.scalatest" %% "scalatest" % "1.6.1" % "test"
)

parallelExecution in Test := false
