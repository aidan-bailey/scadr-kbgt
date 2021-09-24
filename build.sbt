scalaVersion := "2.13.6"

name := "kbgt"
organization := "University of Cape Town"
version := "1.0"

libraryDependencies += "com.github.scopt" % "scopt_2.13" % "4.0.1"

Compile / doc / scalacOptions := Seq("-groups", "-implicits")
