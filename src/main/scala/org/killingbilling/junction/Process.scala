package org.killingbilling.junction

import java.io.{OutputStreamWriter, PrintStream}
import java.nio.charset.Charset
import java.nio.file.Paths
import scala.beans.BeanProperty

object Process {

  @BeanProperty val noDeprecation = false
  @BeanProperty val throwDeprecation = true
  @BeanProperty val traceDeprecation = true
  // not really needed, because throwDeprecation == true

  @BeanProperty val platform = System.getProperty("os.name").toLowerCase
  @BeanProperty val env = System.getenv()

  def chdir(d: String) {
    throw new RuntimeException(s"Error: JVM cannot change process current dir")
  }

  private val _cwd: String = Paths.get(".").toAbsolutePath.normalize().toString
  def cwd(): String = _cwd

  class Writer(stream: PrintStream) {
    private val writer = new OutputStreamWriter(stream, Charset.defaultCharset())
    def write(s: String) = {writer.write(s); writer.flush()}
  }

  // these are to implement write(s: String) // TODO impl
  @BeanProperty lazy val stdout = new Writer(Console.out)
  @BeanProperty lazy val stderr = new Writer(Console.err)

}
