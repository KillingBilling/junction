package org.killingbilling.junction

import javax.script.ScriptEngine
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

      // TODO resolve and load
      m.exports.put("dummyID", path)

      m.id = path // TODO impl
      m.filename = path // TODO impl

      self.children.add(m)
      m.loaded = true

      m.exports
    }

  }

  def getRequire: JFunction[String, JsObject] with (String => JsObject) = _require

  var id = ""

  var filename = ""

  var loaded = false

  def getParent: Module = parent getOrElse null

  val children: JList[Module] = new JArrayList()

}
