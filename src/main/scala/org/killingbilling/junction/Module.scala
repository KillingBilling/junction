package org.killingbilling.junction

import java.io.{FileReader, File}
import java.util.function.{Function => JFunction}
import java.util.{List => JList, ArrayList => JArrayList, Map => JMap, HashMap => JHashMap}
import javax.script.ScriptContext._
import javax.script.{ScriptContext, SimpleScriptContext, ScriptEngine}
import org.killingbilling.junction.utils._
import scala.beans.BeanInfo

object Module {

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

  def moduleContext(module: Module, root: Option[ScriptContext] = None)
        (implicit engine: ScriptEngine): ScriptContext = {
    val context = newContext()
    initGlobals(module, context, root getOrElse context)
    context
  }

  private def initGlobals(module: Module, context: ScriptContext, rootContext: ScriptContext) {
    val g = context.getBindings(GLOBAL_SCOPE)
    g.put("global", rootContext.getBindings(GLOBAL_SCOPE)) // global
    g.put("process", Process) // global
    //g.put("console", null) // global, require from resources/lib // TODO impl
    //g.put("Buffer", null) // global // TODO impl

    g.put("require", module._require)
    g.put("__filename", module.filename)
    g.put("__dirname", module._dir.toString)
    g.put("module", module)
    g.put("exports", module._exports)
  }

}

@BeanInfo
class Module(parent: Option[Module] = None, val id: String = "[root]")(implicit engine: ScriptEngine) {self =>

  import Module._

  private lazy val rootContext: ScriptContext = parent map {_.rootContext} getOrElse moduleContext(self)

  private var _exports: AnyRef = new JHashMap()
  def getExports: AnyRef = _exports
  def setExports(o: AnyRef) {_exports = o}

  private object _require extends JFunction[String, AnyRef] with Require {

    def apply(path: String) = {
      val module = _resolve(path) map {
        case (true, resolved) => _coreModule(resolved)
        case (false, resolved) => Option(_cache.get(resolved)) getOrElse _loadModule(resolved)
      } getOrElse {
        throw new RuntimeException(s"Error: Cannot find module '$path'")
      }
      module._exports
    }

    def resolve(path: String): String = _resolve(path) map {_._2} getOrElse {
      throw new RuntimeException(s"Error: Cannot find module '$path'")
    }

    private def _resolve(path: String): Option[(Boolean, String)] = {
      if (path.startsWith(".") || path.startsWith("/")) {
        val file = new File(path)
        val absPath = (if (file.isAbsolute) file else new File(_dir, path)).getCanonicalPath
        List("", ".js", ".json") map {ext => new File(absPath + ext)} collectFirst {
          case f if f.isFile => (false, f.toString)
          case f if f.isDirectory => ??? // TODO impl
        }
      } else if (isCore(path)) (true, path) else inNodeModules(path) map {false -> _}
    }

    private def inNodeModules(path: String): Option[String] = ??? // TODO impl

    def getCache: JMap[String, Module] = _cache // global, map: id -> module

    private def _loadModule(resolved: String): Module = {
      val module = new Module(self, resolved)
      _cache.put(resolved, module)
      val context = moduleContext(module, rootContext)

      engine.eval(new FileReader(resolved), context)

      self.children.add(module)
      module._loaded = true
      module
    }

    private def isCore(path: String): Boolean =
      List(
        "_linklist", "process", "console", "util", "sys", "punycode", "url", "querystring", "console"
      ).contains(path)

    private def _coreModule(path: String): Module = ??? // TODO impl

  }

  private val _cache: JMap[String, Module] = parent map {_._cache} getOrElse new JHashMap()

  def getRequire: JFunction[String, AnyRef] = _require

  val filename: String = new File(id).getCanonicalPath
  private val _dir: File = new File(filename).getParentFile

  private var _loaded = false
  def isLoaded = _loaded

  def getParent: Module = parent getOrElse null

  val children: JList[Module] = new JArrayList()

}
