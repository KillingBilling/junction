package org.killingbilling.junction

import java.util.{Map => JMap}
import org.killingbilling.junction.utils._

trait Require {
  def apply(path: String): AnyRef

  def resolve(path: String): String
  def getCache: JMap[String, Module]

  def impl[T](path: String, c: Class[T]): T
}

object Require {

  def apply(): Require = new Module().getRequire

}
