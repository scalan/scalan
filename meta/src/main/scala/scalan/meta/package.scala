/**
 * User: Alexander Slesarenko
 * Date: 12/15/13
 */
import java.io.PrintWriter
import scala.io.Source
package scalan {
  package object meta {

    def writeFile(name: String, text: String) = {
      val p = new PrintWriter(name)
      p.print(text)
      p.close()
    }

  }
}