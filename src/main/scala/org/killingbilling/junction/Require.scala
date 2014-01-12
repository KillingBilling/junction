package org.killingbilling.junction

import java.util.{Map => JMap}
import org.killingbilling.junction.utils._

trait Require {
  def resolve(path: String): String
  def getCache: JMap[String, Module]
  def apply(path: String): AnyRef
}

object Require extends Require {

  private lazy val default: Require = forModule(new Module()(newEngine()))

  def getCache = default.getCache
  def resolve(path: String) = default.resolve(path)
  def apply(path: String) = default.apply(path)


  def forModule(module: Module): Require = module.getRequire

}
