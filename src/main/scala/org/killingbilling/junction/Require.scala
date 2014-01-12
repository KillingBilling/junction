package org.killingbilling.junction

import java.util.{Map => JMap}
import org.killingbilling.junction.utils._

trait Require {
  def resolve(path: String): String
  def getCache: JMap[String, Module]
  protected def module(path: String): Module

  def impl[T](path: String, c: Class[T]): T

  def apply(path: String): AnyRef = module(path).getExports
}

object Require extends Require {

  private lazy val default: Require = forModule(new Module())

  def getCache = default.getCache
  def resolve(path: String) = default.resolve(path)
  protected def module(path: String) = default.module(path)
  def impl[T](path: String, t: Class[T]) = default.impl(path, t)

  def forModule(module: Module): Require = module.getRequire

}
