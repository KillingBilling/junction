package org.killingbilling.junction

import java.io.File
import java.util.function.{Function => JFunction}
import java.util.{List => JList, ArrayList => JArrayList, Map => JMap, HashMap => JHashMap}
import javax.script.ScriptContext._
import javax.script.{Invocable, ScriptContext, SimpleScriptContext, ScriptEngine}
import org.killingbilling.junction.utils._
import scala.beans.BeanInfo
import scala.io.Source
import scala.util.Try
import scala.util.parsing.json.JSON

object Module {

  def moduleContext(module: Module, rootContext: Option[ScriptContext] = None)
        (implicit engine: ScriptEngine): ScriptContext = {

    val context = new SimpleScriptContext

    context.setBindings(engine.createBindings(), GLOBAL_SCOPE)
    context.setBindings(engine.createBindings(), ENGINE_SCOPE)

    initGlobals(module, context, rootContext getOrElse context)

    context
  }

  private def initGlobals(module: Module, context: ScriptContext, rootContext: ScriptContext) {
    val g = context.getBindings(GLOBAL_SCOPE)
    g.put("global", rootContext.getBindings(GLOBAL_SCOPE)) // global
    g.put("process", Process) // global
    val console = module._require("console")
    g.put("console", console) // global
    g.put("Buffer", Buffer) // global

    g.put("require", module._require)
    g.put("__filename", module.filename)
    g.put("__dirname", module._dir.getPath)
    g.put("module", module)
    g.put("exports", module._exports)
  }

}

@BeanInfo
class Module(parent: Option[Module] = None, val id: String = "[root]")(implicit createEngine: () => ScriptEngine) {self =>

  import Module._

  private lazy implicit val engine: ScriptEngine = parent map {_.engine} getOrElse createEngine()

  private lazy val root: Module = parent map {_.root} getOrElse self
  private lazy val context: ScriptContext =
    parent map {_ => moduleContext(self, root.context)} getOrElse moduleContext(self)

  private var _exports: AnyRef = new JHashMap()
  def getExports: AnyRef = _exports
  def setExports(o: AnyRef) {_exports = o}

  def invocable = {
    val engine = newEngine()
    engine.setContext(context)
    engine.asInstanceOf[Invocable]
  }

  private object _require extends JFunction[String, AnyRef] with Require {

    def module(path: String) = {
      val module = _resolve(path)(_dir) map {
        case (true, resolved) => Option(_core.get(resolved)) getOrElse _coreModule(resolved)
        case (false, resolved) => Option(_cache.get(resolved)) getOrElse _loadModule(resolved)
      } getOrElse {
        throw new RuntimeException(s"Error: Cannot find module '$path'")
      }
      module
    }

    def resolve(path: String): String = _resolve(path)(_dir) map {_._2} getOrElse {
      throw new RuntimeException(s"Error: Cannot find module '$path'")
    }

    private def _resolve(path: String)(dir: File): Option[(Boolean, String)] = {
      if (path.startsWith(".") || path.startsWith("/")) {
        val file = new File(path)
        val absPath = (if (file.isAbsolute) file else new File(dir, path)).getCanonicalPath
        (List("", ".js", ".json") map {ext => new File(absPath + ext)} collectFirst {
          case f if f.isFile => Some(false -> f.getPath)
          case f if f.isDirectory => resolveDir(f)
        }).flatten
      } else if (isCore(path)) (true, path) else inNodeModules(path)(dir)
    }

    private def resolveDir(dir: File): Option[(Boolean, String)] = {
      val main = Try {
        val opt = JSON.parseFull(Source.fromFile(new File(dir, "package.json")).mkString)
        opt.get.asInstanceOf[Map[String, String]]("main")
      }.toOption getOrElse "./index.js"
      _resolve(main)(dir)
    }

    private def inNodeModules(path: String)(dir: File): Option[(Boolean, String)] = {
      if (dir == null) None
      else _resolve(new File(new File(dir, "node_modules"), path).getPath)(dir) match {
        case s@Some(_) => s
        case None => inNodeModules(path)(dir.getParentFile)
      }
    }

    def getCache: JMap[String, Module] = _cache // global, map: id -> module

    private def _loadModule(resolved: String): Module = {
      val module = new Module(self, resolved)
      _cache.put(resolved, module)

      val Ext = """.*(\.\w+)$""".r
      resolved match {
        case Ext(".json") =>
          module._exports = (Try {
            import scala.collection.JavaConversions._
            JSON.parseFull(Source.fromFile(resolved).mkString).get match {
              case a: Map[String, AnyRef] => a: JMap[String, AnyRef]
              case a: List[AnyRef] => a: JList[AnyRef]
            }
          } recover {
            case e => throw new RuntimeException(s"JSON parse error: $resolved", e)
          }).get
        case Ext(".js") | _ =>
          engine.eval(Source.fromFile(resolved).bufferedReader(), module.context)
      }

      self.children.add(module)
      module._loaded = true
      module
    }

    private def isCore(path: String): Boolean =
      List(
        "_linklist", "assert", "console", "freelist", "path", "punycode", "querystring", "sys", "url", "util"
      ).contains(path)

    private def _coreModule(resolved: String): Module = {
      val module = new Module(root, resolved)
      _core.put(resolved, module)
      val inputStream = getClass.getClassLoader.getResourceAsStream(s"lib/$resolved.js")
      engine.eval(Source.fromInputStream(inputStream).bufferedReader(), module.context)
      root.children.add(module)
      module._loaded = true
      module
    }

  }

  private val _cache: JMap[String, Module] = parent map {_._cache} getOrElse new JHashMap()
  private val _core: JMap[String, Module] = parent map {_._core} getOrElse new JHashMap()

  def getRequire: JFunction[String, AnyRef] with Require = _require

  val filename: String = new File(id).getCanonicalPath
  private val _dir: File = new File(filename).getParentFile

  private var _loaded = false
  def isLoaded = _loaded

  def getParent: Module = parent getOrElse null

  val children: JList[Module] = new JArrayList()

}
