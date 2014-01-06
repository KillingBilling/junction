package org.killingbilling.junction

import java.util.function.{Function => JFunction}
import java.util.{Map => JMap, List => JList}
import javax.script._

import Require._

object Require {

  def freshModule(currentModule: JMap[String, AnyRef]): JMap[String, AnyRef] = {
    val m = new java.util.HashMap[String, AnyRef]
    m.put("exports", new java.util.HashMap[String, AnyRef])
    m.put("loaded", Boolean.box(false))
    m.put("parent", currentModule)
    m.put("children", new java.util.ArrayList())
    m
  }

}

class Require(currentModule: JMap[String, AnyRef] = utils.newRootModule)(implicit engine: ScriptEngine)
  extends JFunction[String, AnyRef] with (String => AnyRef) {

  currentModule.put("require", this)

  private def freshModuleBindings(): (JMap[String, AnyRef], Bindings) = {
    val m = freshModule(currentModule)
    val g = engine.createBindings()

    g.put("module", m)
    g.put("require", new Require(m))
    g.put("exports", m.get("exports"))

    (m, g)
  }

  def apply(path: String): AnyRef = {
    val (m, g) = freshModuleBindings()

    m.put("id", path) // FIXME resolved path
    m.put("filename", path) // FIXME resolved path

    engine.eval( s"""module.exports = "MODULE_$path";""", g) // TODO replace stub

    m.put("loaded", Boolean.box(true))
    currentModule.get("children").asInstanceOf[JList[AnyRef]].add(m)
    m.get("exports")
  }

}
