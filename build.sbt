name := "bamboo"

//version := "0.9.25"
version := "0.9.25-SNAPSHOT"

scalaVersion := "2.12.8"

crossScalaVersions := Seq("2.11.12", "2.12.8", "2.13.0")

// ALL = Do counts,  OFF = No counts
// scalacOptions ++= Seq( "-deprecation", "-unchecked", "-Xelide-below", "OFF" )
scalacOptions ++= Seq( "-deprecation", "-unchecked", "-feature", "-Xelide-below", "ALL" )

licenses := List(("Apache-2.0",url("https://www.apache.org/licenses/LICENSE-2.0")))

