package org.killingbilling.junction

import java.util.function.{Function => JFunction}
import java.util.{List => JList, ArrayList => JArrayList, Map => JMap, HashMap => JHashMap}
import javax.script.ScriptContext._
import javax.script.{ScriptContext, SimpleScriptContext, ScriptEngine}
import org.killingbilling.junction.utils._
import scala.beans.BeanInfo
import java.io.File

object Module {

  type JsObject = JMap[String, AnyRef]

  trait Require {
    def resolve(path: String): String
    def getCache: JMap[String, Module]
  }

  def newContext()(implicit engine: ScriptEngine): ScriptContext = {
    val context = new SimpleScriptContext
    context.setBindings(engine.createBindings(), GLOBAL_SCOPE)
    context.setBindings(engine.createBindings(), ENGINE_SCOPE)
    context
  }

  def moduleContext(module: Module, root: Option[ScriptContext] = None)(implicit engine: ScriptEngine): ScriptContext = {
    val context = newContext()
    initGlobals(module, context, root getOrElse context)
    context
  }

  private def initGlobals(module: Module, context: ScriptContext, rootContext: ScriptContext) {
    val g = context.getBindings(GLOBAL_SCOPE)
    g.put("global", rootContext.getBindings(GLOBAL_SCOPE)) // global
    g.put("process", Process) // global
    val require = module.getRequire
    //g.put("console", null) // global, require from resources/lib // TODO impl
    //g.put("Buffer", null) // global // TODO impl

    g.put("require", require)
    g.put("__filename", module.filename)
    g.put("__dirname", module._dir.toString)
    g.put("module", module)
    g.put("exports", module.exports)
  }

}

@BeanInfo
class Module(parent: Option[Module] = None, val id: String = "[root]")(implicit engine: ScriptEngine) {self =>

  import Module._

  private val rootContext: ScriptContext = parent map {_.rootContext} getOrElse moduleContext(self)

  var exports: JsObject = new JHashMap()

  private object _require extends JFunction[String, JsObject] with (String => JsObject) with Require {

    def apply(path: String) = {
      val filename = resolve(path)
      val module = Option(_cache.get(filename)) getOrElse loadModule(filename)
      module.exports
    }

    def resolve(path: String): String = {
      (if (path.startsWith(".") || path.startsWith("/")) {
        val file = new File(path)
        val filename = (if (file.isAbsolute) file else new File(_dir, path)).getCanonicalPath
        List("", ".js", ".json") collectFirst {
          case ext if new File(filename + ext).exists() => filename + ext
        }
      } else None) getOrElse {
        throw new RuntimeException(s"Module $path does not exist!")
      }
    }

    def getCache: JMap[String, Module] = _cache // global, map: id -> module

    private def loadModule(path: String) = {
      val module = new Module(self, path)
      val context = moduleContext(module, rootContext)

      engine.eval(s"exports.dummyID = '$path';", context) // TODO impl load

      self.children.add(module)
      module._loaded = true
      module
    }

  }
  
  private val _cache: JMap[String, Module] = parent map {_._cache} getOrElse new JHashMap()

  def getRequire: JFunction[String, JsObject] with (String => JsObject) with Require = _require

  val filename: String = new File(id).getCanonicalPath
  private val _dir: File = new File(filename).getParentFile

  private var _loaded = false
  def isLoaded = _loaded

  def getParent: Module = parent getOrElse null

  val children: JList[Module] = new JArrayList()

}
