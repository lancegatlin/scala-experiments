scalaVersion := "2.10.0"

organization := "org.lancegatlin"

name := "parsing"

version := "1.0-SNAPSHOT"

scalacOptions ++= Seq("-feature","-unchecked", "-deprecation")

resolvers += "Mandubian repository snapshots" at "https://github.com/mandubian/mandubian-mvn/raw/master/snapshots/"

resolvers += "Mandubian repository releases" at "https://github.com/mandubian/mandubian-mvn/raw/master/releases/"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.0.M5b"

libraryDependencies += "play" %% "play-json" % "2.2-SNAPSHOT"

libraryDependencies += "org.specs2" %% "specs2" % "1.13" % "test"

libraryDependencies += "junit" % "junit" % "4.8" % "test"

