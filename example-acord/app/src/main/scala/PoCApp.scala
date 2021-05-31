import com.digitalasset.app.Commands

import java.nio.file.Paths

object PoCApp {
  def main(args: Array[String]): Unit = {
    Commands.init()
    val xml = Paths.get(args(0)).toUri.toURL
    Commands.send(xml)
  }
}
