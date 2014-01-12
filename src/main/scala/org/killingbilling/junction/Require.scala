package org.killingbilling.junction

import java.util.{Map => JMap}
import org.killingbilling.junction.utils._

trait Require {
  def resolve(path: String): String
  def getCache: JMap[String, Module]
  def module(path: String): Module

  def impl[T](path: String, c: Class[T]): T

  def apply(path: String): AnyRef = module(path).getExports
}

object Require {

  def apply(): Require = forModule(new Module())

  def forModule(module: Module): Require = module.getRequire

}
