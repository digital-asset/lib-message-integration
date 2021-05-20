import sbt.{AutoPlugin, File}

import scala.sys.process._

object DamlPlugin extends AutoPlugin {
  object autoImport {}

  case class DamlCodegenSettings(dar: File,
                                 outputDirectory: File,
                                 packagePrefix: Option[String] = None,
                                 decoderClass: Option[String] = None)

  def damlCodegen(settings: DamlCodegenSettings): Int = {
    val codegen = Seq.newBuilder[String]
    codegen += ("daml", "codegen", "java")
    codegen += ("--output-directory", settings.outputDirectory.toString)
    settings.decoderClass.foreach { x =>
      codegen += "--decoderClass"
      codegen += x
    }
    settings.packagePrefix match {
      case Some(packagePrefix) => codegen += s"${settings.dar}=$packagePrefix"
      case None => codegen += settings.dar.toString
    }
    codegen.result().!
  }
}
