package nl.codestar.scalatsi.output

import java.io.FileWriter

import nl.codestar.scalatsi.{TSNamedType, TypescriptTypeSerializer}

object WriteTSToFiles {
  def write(options: OutputOptions)(types: Seq[TSNamedType[_]]): Unit = {
    val output = TypescriptTypeSerializer.emits(types: _*)
    if (!options.targetFile.exists()) {
      options.targetFile.createNewFile()
    }
    val writer = new FileWriter(options.targetFile)
    writer.write(output)
    writer.close()
  }
}
