name := "bamboo"

//version := "0.9.24"
version := "0.9.24-SNAPSHOT"

scalaVersion := "2.12.7"

crossScalaVersions := Seq("2.11.12", "2.12.7")

// ALL = Do counts,  OFF = No counts
// scalacOptions ++= Seq( "-deprecation", "-unchecked", "-Xelide-below", "OFF" )
scalacOptions ++= Seq( "-deprecation", "-unchecked", "-feature", "-Xelide-below", "ALL" )

