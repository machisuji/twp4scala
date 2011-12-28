name := "twp4scala"

version := "0.1"

scalaVersion := "2.9.1"

mainClass := Some("twp4scala.example.Hello")

scalacOptions ++= Seq("-deprecation", "-unchecked")

libraryDependencies ++= Seq(
  "dnsjava" % "dnsjava"% "2.1.1"
)
