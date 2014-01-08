package org.killingbilling.junction

import java.util.function
import javax.script.ScriptEngineManager

package object utils {

  def jf[A, B](f: Function[A, B]) = new function.Function[A, B] {
    def apply(t: A) = f(t)
  }

  implicit def sf[A, B](f: function.Function[A, B]): A => B = f.apply

  implicit def a2opt[A](a: A): Option[A] = Option(a)

  implicit lazy val sem = new ScriptEngineManager()

  def newEngine(name: String = "nashorn")(implicit sem: ScriptEngineManager) = {
    sem.getEngineByName(name)
  }

  def newRootModule = Require.freshModule(null)

}
