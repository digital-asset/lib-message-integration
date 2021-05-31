import com.digitalasset.app.Commands

import java.nio.file.Paths

object PoCApp {
  def main(args: Array[String]): Unit = {
    Commands.init()
    val xml = Paths.get("lib-integration-java/src/main/resources/acord/TL_103.xml").toUri.toURL
    Commands.send(xml)
  }
}
