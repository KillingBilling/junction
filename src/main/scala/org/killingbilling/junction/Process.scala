package org.killingbilling.junction

import java.io.{File, OutputStreamWriter, PrintStream}
import java.nio.charset.Charset
import scala.beans.BeanProperty

object Process {

  @BeanProperty val noDeprecation = false
  @BeanProperty val throwDeprecation = true
  @BeanProperty val traceDeprecation = true
  // not really needed, because throwDeprecation == true

  @BeanProperty val platform = System.getProperty("os.name").toLowerCase
  @BeanProperty val env = System.getenv()

  def chdir(d: String) {
    val dir = new File(d).getCanonicalFile
    if (dir.isDirectory) {
      System.setProperty("user.dir", dir.getPath)
    } else throw new RuntimeException(s"Error: no such directory: ${dir.getPath}")
  }

  def cwd(): String = System.getProperty("user.dir")
  // == new File(".").getCanonicalPath

  class Writer(stream: PrintStream) {
    private val writer = new OutputStreamWriter(stream, Charset.defaultCharset())
    def write(s: String) = {writer.write(s); writer.flush()}
  }

  // these are to implement write(s: String) // TODO impl
  @BeanProperty lazy val stdout = new Writer(Console.out)
  @BeanProperty lazy val stderr = new Writer(Console.err)

}
