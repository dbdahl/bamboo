name := "bamboo"

version := "0.9.26"
//version := "0.9.26-SNAPSHOT"

scalaVersion := "2.13.1"
crossScalaVersions := Seq("2.11.12", "2.12.11", "2.13.1")
scalacOptions ++= Seq( "-deprecation", "-unchecked", "-feature", "-Xelide-below", "ALL" )

