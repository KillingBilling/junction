package org.killingbilling.junction

import java.io.{Reader, File}
import java.util.function.{Function => JFunction}
import java.util.{List => JList, ArrayList => JArrayList, Map => JMap, HashMap => JHashMap}
import javax.script.ScriptContext._
import javax.script.{Bindings, Invocable, ScriptEngine}
import org.killingbilling.junction.utils._
import scala.beans.BeanInfo
import scala.io.Source
import scala.util.Try
import scala.util.parsing.json.JSON

object Module {

  case class WithObj[T](module: Module, obj: Option[T])
  
  case class Context(engine: ScriptEngine, globals: Bindings, locals: Bindings) {

    def createBindings() = engine.createBindings()

    private def swapGlobals(newGlobals: Bindings): Bindings = {
      val oldGlobals = engine.getBindings(GLOBAL_SCOPE)
      engine.setBindings(newGlobals, GLOBAL_SCOPE)
      oldGlobals
    }

    def eval[T](source: Reader, tOpt: Option[Class[T]]): Option[T] = {
      val old = swapGlobals(globals)
      engine.eval(source, locals)
      
      val obj = tOpt map {t =>
        engine.eval("var __exports = module.exports;", locals)
        val inv = engine.asInstanceOf[Invocable]
        inv.getInterface(locals.get("__exports"), t)
      } flatMap {v => Option(v)}
      
      swapGlobals(old)
      
      obj
    }

  }

  object Context {
    def apply(engine: ScriptEngine): Context = Context(engine, engine.createBindings(), engine.createBindings())
  }

  def moduleContext(module: Module, rootContext: Option[Context] = None)(implicit engine: ScriptEngine): Context = {
    val context = Context(engine)
    initGlobals(module, context, rootContext getOrElse context)
    context
  }

  private def initGlobals(module: Module, context: Context, rootContext: Context) {
    val g = context.globals
    g.put("global", rootContext.globals) // global
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
  private lazy val context: Context =
    parent map {_ => moduleContext(self, root.context)} getOrElse moduleContext(self)

  private var _exports: AnyRef = new JHashMap()
  def getExports: AnyRef = _exports
  def setExports(o: AnyRef) {_exports = o}

  private object _require extends JFunction[String, AnyRef] with Require {

    protected def moduleWithObj[T](path: String, t: Option[Class[T]]): WithObj[T] = {
      val moduleWithObj: WithObj[T] = _resolve(path)(_dir) map {
        case (true, resolved) => Option(_core.get(resolved)) map {WithObj[T](_, None)} getOrElse _coreModule(resolved, t)
        case (false, resolved) => Option(_cache.get(resolved)) map {WithObj[T](_, None)} getOrElse _loadModule(resolved, t)
      } getOrElse {
        throw new RuntimeException(s"Error: Cannot find module '$path'")
      }
      moduleWithObj
    }

    def apply(path: String): AnyRef = moduleWithObj(path, None).module.getExports

    def impl[T](path: String, t: Class[T]): Option[T] = moduleWithObj(path, t).obj

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

    private def _loadModule[T](resolved: String, t: Option[Class[T]]): WithObj[T] = {
      val module = new Module(self, resolved)
      _cache.put(resolved, module)

      val Ext = """.*(\.\w+)$""".r
      val obj: Option[T] = resolved match {
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
          None
        case Ext(".js") | _ =>
          module.context.eval(Source.fromFile(resolved).bufferedReader(), t)
      }

      self.children.add(module)
      module._loaded = true
      WithObj[T](module, obj)
    }

    private def isCore(path: String): Boolean =
      List(
        "_linklist", "assert", "console", "freelist", "path", "punycode", "querystring", "sys", "url", "util"
      ).contains(path)

    private def _coreModule[T](resolved: String, t: Option[Class[T]]): WithObj[T] = {
      val module = new Module(root, resolved)
      _core.put(resolved, module)
      val inputStream = getClass.getClassLoader.getResourceAsStream(s"lib/$resolved.js")
      val obj: Option[T] = module.context.eval(Source.fromInputStream(inputStream).bufferedReader(), t)
      root.children.add(module)
      module._loaded = true
      WithObj[T](module, obj)
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
