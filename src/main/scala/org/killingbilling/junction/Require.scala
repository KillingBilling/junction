package org.killingbilling.junction

import java.util.{Map => JMap}
import org.killingbilling.junction.utils._

trait Require {
  def resolve(path: String): String
  def getCache: JMap[String, Module]
  def module(path: String): Module
  def apply(path: String): AnyRef = module(path).getExports
}

object Require extends Require {

  private lazy val default: Require = forModule(new Module())

  def getCache = default.getCache
  def resolve(path: String) = default.resolve(path)
  def module(path: String) = default.module(path)

  def forModule(module: Module): Require = module.getRequire

}
