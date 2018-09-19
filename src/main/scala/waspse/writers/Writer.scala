package waspse.writers

import java.io.PrintWriter
import java.nio.file.{FileSystems, Files, LinkOption}

trait Writer {

  private def OutputDir = "output"
  private def PresetsDir = "presets"

  protected def getPrintWriter(name: String, `type`: String, extension: String): PrintWriter = {
    val fs = FileSystems.getDefault

    val filePath = fs.getPath(OutputDir, `type`, name + extension)
    val linkPath = fs.getPath(PresetsDir, name, `type` + extension)
    val linkTarget = linkPath.getParent.relativize(filePath)

    if (!Files.exists(linkPath, LinkOption.NOFOLLOW_LINKS)) {
      Files.createDirectories(linkPath.getParent)
      Files.createSymbolicLink(linkPath, linkTarget)
    }

    Files.createDirectories(filePath.getParent)
    new PrintWriter(filePath.toFile)
  }
}
