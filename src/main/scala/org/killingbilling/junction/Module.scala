package org.killingbilling.junction

import javax.script.ScriptEngine
import scala.beans.BeanProperty
import org.killingbilling.junction.utils._
import java.util.function.{Function => JFunction}
import java.util.{List => JList, ArrayList => JArrayList, Map => JMap, HashMap => JHashMap}

class Module(parent: Option[Module] = None)(implicit engine: ScriptEngine) {self =>

  @BeanProperty var exports: JMap[String, AnyRef] = new JHashMap()

  private object _require extends JFunction[String, AnyRef] with (String => AnyRef) {

    def apply(path: String): AnyRef = {
      val m = new Module(self)

      // TODO resolve and load
      m.exports.put("dummyID", path)

      m.id = path  // TODO impl

      m.loaded = true
      m.exports
    }

  }

  def getRequire: JFunction[String, AnyRef] with (String => AnyRef) = _require

  @BeanProperty var id = ""

  @BeanProperty var loaded = false

  def getParent = parent getOrElse null

  @BeanProperty val children: JList[AnyRef] = new JArrayList() // TODO impl

}
