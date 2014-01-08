package org.killingbilling.junction

import java.util.function.{Function => JFunction}
import java.util.{List => JList, ArrayList => JArrayList, Map => JMap, HashMap => JHashMap}
import javax.script.ScriptContext._
import javax.script.{ScriptContext, SimpleScriptContext, ScriptEngine}
import org.killingbilling.junction.utils._
import scala.beans.BeanInfo

object Module {

  type JsObject = JMap[String, AnyRef]

  def newContext()(implicit engine: ScriptEngine): ScriptContext = {
    val context = new SimpleScriptContext
    context.setBindings(engine.createBindings(), GLOBAL_SCOPE)
    context.setBindings(engine.createBindings(), ENGINE_SCOPE)
    context
  }

  def initGlobals(module: Module, context: ScriptContext, rootContext: ScriptContext) {
    val g = context.getBindings(GLOBAL_SCOPE)
    g.put("global", rootContext.getBindings(GLOBAL_SCOPE))
    g.put("process", Process) // global
    val require = module.getRequire
    //g.put("console", null) // global, require from resources/lib
    //g.put("__filename", null)
    //g.put("__dirname", null)
    g.put("require", require)
    g.put("module", module)
    g.put("exports", module.exports)
  }

}

@BeanInfo
class Module(parent: Option[Module] = None)(implicit engine: ScriptEngine) {self =>

  import Module._

  private val rootContext: ScriptContext = parent map {_.rootContext} getOrElse {
    val context = newContext()
    initGlobals(self, context, context)
    context
  }

  var exports: JsObject = new JHashMap()

  private object _require extends JFunction[String, JsObject] with (String => JsObject) {

    def apply(path: String) = {
      val module = new Module(self)

      val context = newContext()
      initGlobals(module, context, rootContext)

      engine.eval(s"exports.dummyID = '$path';", context) // TODO impl load

      module.id = path // TODO impl
      module.filename = path // TODO impl

      self.children.add(module)
      module.loaded = true

      module.exports
    }

    def resolve(path: String): String = ???

    def getCache = ???

  }

  def getRequire: JFunction[String, JsObject] with (String => JsObject) = _require

  var id = ""

  var filename = ""

  var loaded = false

  def getParent: Module = parent getOrElse null

  val children: JList[Module] = new JArrayList()

}
