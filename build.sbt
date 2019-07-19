scalaVersion in ThisBuild := "2.12.8"

lazy val commonSettings = Seq(
  organization := "com.example",
  version := "0.1.0-SNAPSHOT"
)

lazy val app = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    name := "pdf-parser"
  ).
  enablePlugins(AssemblyPlugin)

resolvers in Global ++= Seq(
  "Sbt plugins"                   at "https://dl.bintray.com/sbt/sbt-plugin-releases",
  "Maven Central Server"          at "http://repo1.maven.org/maven2",
  "TypeSafe Repository Releases"  at "http://repo.typesafe.com/typesafe/releases/",
  "TypeSafe Repository Snapshots" at "http://repo.typesafe.com/typesafe/snapshots/"
)

libraryDependencies ++= Seq(
  "org.apache.pdfbox" % "pdfbox" % "2.0.13",
  "org.scala-lang.modules" %% "scala-xml" % "1.1.1",
  "org.json4s" %% "json4s-native" % "3.6.5",
  "org.json4s" %% "json4s-xml" % "3.6.5",
  "com.typesafe.play" %% "play-json" % "2.7.3",
  "com.github.pathikrit" %% "better-files" % "3.8.0"
)