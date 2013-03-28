scalaVersion := "2.10.0"

organization := "org.lancegatlin"

name := "primes"

version := "1.0-SNAPSHOT"

scalacOptions ++= Seq("-feature","-unchecked", "-deprecation")

libraryDependencies += "org.scalatest" %% "scalatest" % "2.0.M5b"
