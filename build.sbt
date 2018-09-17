name := "bamboo"

version := "0.9.23-SNAPSHOT"

scalaVersion := "2.12.6"

crossScalaVersions := Seq("2.11.12", "2.12.6")

// ALL = Do counts,  OFF = No counts
// scalacOptions ++= Seq( "-deprecation", "-unchecked", "-Xelide-below", "OFF" )
scalacOptions ++= Seq( "-deprecation", "-unchecked", "-feature", "-Xelide-below", "ALL" )

