name := "winamp-sps-extract"

version := "0.1"

scalaVersion := "2.12.6"


lazy val extractor = (project in file("extractor"))
  .settings(
    libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.1"
  )
