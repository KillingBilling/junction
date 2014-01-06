package org.killingbilling.junction

import java.util.function.{Function => JFunction}
import javax.script._

class Require(implicit engine: ScriptEngine) extends JFunction[String, AnyRef] with (String => AnyRef) {

  private def newBindings(): Bindings = {
    val g = engine.createBindings()

    g.put("require", new Require)

    val exports = new java.util.HashMap[String, AnyRef]
    g.put("exports", exports)

    val module = new java.util.HashMap[String, AnyRef]
    module.put("exports", exports)

    g.put("module", module)

    g
  }

  def apply(path: String): AnyRef = {
    val bindings = newBindings()
    // TODO replace stub
    engine.eval(s"""module.exports = "MODULE_${path}_";""", bindings)
    bindings.get("module").asInstanceOf[java.util.Map[String, AnyRef]].get("exports")
  }

}
