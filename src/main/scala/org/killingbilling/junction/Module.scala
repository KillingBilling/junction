package org.killingbilling.junction

import javax.script.{ScriptContext, ScriptEngine}
import scala.beans.BeanInfo
import org.killingbilling.junction.utils._
import java.util.function.{Function => JFunction}
import java.util.{List => JList, ArrayList => JArrayList, Map => JMap, HashMap => JHashMap}

object Module {

  type JsObject = JMap[String, AnyRef]

}

@BeanInfo
class Module(parent: Option[Module] = None)(implicit engine: ScriptEngine) {self =>

  import Module._

  var exports: JsObject = new JHashMap()

  private object _require extends JFunction[String, JsObject] with (String => JsObject) {

    def apply(path: String) = {
      val m = new Module(self)

      val g = engine.createBindings()
      g.put("global", engine.getBindings(ScriptContext.GLOBAL_SCOPE))
      g.put("process", Process)
      val require = m.getRequire
      g.put("console", require("console")) // TODO impl require from resources/lib
      g.put("__filename", ???)
      g.put("__dirname", ???)
      g.put("require", require)
      g.put("module", m)
      g.put("exports", m.exports)

      engine.eval(s"""exports.dummyID = '$path';""", g) // TODO impl load

      m.id = path // TODO impl
      m.filename = path // TODO impl

      self.children.add(m)
      m.loaded = true

      m.exports
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
