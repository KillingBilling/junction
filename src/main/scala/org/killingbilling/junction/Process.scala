package org.killingbilling.junction

import java.io.{OutputStreamWriter, PrintStream}
import java.nio.charset.Charset
import scala.beans.BeanProperty

object Process {

  @BeanProperty val noDeprecation = false
  @BeanProperty val throwDeprecation = true
  @BeanProperty val traceDeprecation = true
  // not really needed, because throwDeprecation == true

  class FlushWriter(stream: PrintStream) extends OutputStreamWriter(stream, Charset.defaultCharset()) {
    override def write(s: String) = {super.write(s); flush()}
  }

  // these are to implement write(s: String) // TODO impl
  @BeanProperty lazy val stdout = new FlushWriter(Console.out)
  @BeanProperty lazy val stderr = new FlushWriter(Console.err)

}
