import scala.sys.process.Process

name := "winamp-sps-extract"

version := "0.1"

scalaVersion := "2.12.7"

lazy val root = (project in file("."))
  .aggregate(extractor, player, symbolicExecutor)

lazy val extractor = (project in file("extractor"))
  .settings(
    libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.1"
  )

lazy val player = (project in file("player"))
  .settings(
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value,
    Compile / sourceGenerators += Def.task {
      val extractorJarFile = (extractor / Compile / packageBin).value
//      val rootBaseDirectory = (root / baseDirectory).value // Causes stack overflow :-(
      val rootBaseDirectory = baseDirectory.value.getParentFile
      for (spsFile <- (rootBaseDirectory / "DSP_SPS").listFiles().toSeq)
      yield {
        generateSource(
          spsFile,
          sourceManagedDir = (Compile / sourceManaged).value,
          rootBaseDir = rootBaseDirectory,
          extractorJarFile = extractorJarFile,
        )
      }
    }.taskValue
  )

def generateSource(spsFile: File, sourceManagedDir: File, rootBaseDir: File, extractorJarFile: File): File = {
  val spsName = spsFile.getName
  assert(spsName endsWith ".sps")
  val scalaName = spsName.dropRight(4) + ".scala"
  val sourceFile = sourceManagedDir / "typed" / scalaName
  if (!sourceFile.exists() || extractorJarFile.lastModified() > sourceFile.lastModified()) {
    println("Generating " + sourceFile)
    val result = Process(Seq("scala", extractorJarFile.toString, spsFile.toString)).!
    assert(result == 0)
    val generatedFile = rootBaseDir / "output" / "typed" / scalaName
    IO.copyFile(generatedFile, sourceFile)
  }
  sourceFile
}

lazy val symbolicExecutor = (project in file("symbolic-executor"))
  .dependsOn(extractor)
