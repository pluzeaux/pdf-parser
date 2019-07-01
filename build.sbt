name := "pdf-parser-scala2"

version := "0.1"

scalaVersion := "2.12.8"

libraryDependencies ++= Seq(
  "org.apache.pdfbox" % "pdfbox" % "2.0.13",
  "org.scala-lang.modules" %% "scala-xml" % "1.1.1",
  "org.json4s" %% "json4s-native" % "3.6.5",
  "org.json4s" %% "json4s-xml" % "3.6.5",
  "com.typesafe.play" %% "play-json" % "2.7.3",
  "com.github.pathikrit" %% "better-files" % "3.8.0"
)