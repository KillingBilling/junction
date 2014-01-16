package org.killingbilling.junction

import java.io.{Reader, File}
import java.util.function.{Function => JFunction}
import java.util.{List => JList, ArrayList => JArrayList, Map => JMap, HashMap => JHashMap}
import javax.script.ScriptContext._
import javax.script._
import org.killingbilling.junction.utils._
import scala.beans.BeanInfo
import scala.io.{BufferedSource, Source}
import scala.util.Try
import scala.util.parsing.json.JSON

object Module {

  case class WithObj[T](module: Module, obj: Option[T])

  case class Context(module: Module) {

    val locals = module.engine.createBindings()
    val globals = createGlobals()

    globals.put("global", if (module eq module.root) globals else module.root.context.globals)

    private def createGlobals(): Bindings = {
      val g = module.engine.createBindings()
      g.put("process", Process) // global
      g.put("console", module._require("console")) // global
      g.put("Buffer", Buffer) // global

      g.put("require", module._require)
      g.put("__filename", module.filename)
      g.put("__dirname", module._dir.getPath)
      g.put("module", module)
      g
    }

    private def swapGlobals(newGlobals: Bindings): Bindings = {
      val oldGlobals = module.engine.getBindings(GLOBAL_SCOPE)
      module.engine.setBindings(newGlobals, GLOBAL_SCOPE)
      oldGlobals
    }

    def eval[T](source: Reader, tOpt: Option[Class[T]]): Option[T] = {
      val old = swapGlobals(globals)

      try {
        module.singleton.initExports.eval(locals)
        val exports = module.singleton.getExports.eval(locals)
        globals.put("exports", exports)

        module.engine.eval(source, locals) // run!

        val obj = tOpt map {t =>
          val exports = module.singleton.getExports.eval(locals)
          module.engine.asInstanceOf[Invocable].getInterface(exports, t)
        } flatMap {v => Option(v)}

        obj
      } finally {

        swapGlobals(old)
      }
    }

  }

  case class Singleton(root: Module, engine: ScriptEngine) {
    private val comp = engine.asInstanceOf[Compilable]

    val initExports: CompiledScript = comp.compile("'use strict'; module.exports = {};")
    val getExports: CompiledScript = comp.compile("'use strict'; module.exports")
  }

}

@BeanInfo
class Module(val id: String = "[root]", parent: Option[Module] = None)
      (implicit createEngine: () => ScriptEngine) {self =>

  import Module._

  private val singleton: Singleton = parent map {_.singleton} getOrElse Singleton(self, createEngine())

  private val root = singleton.root
  private val engine = singleton.engine

  private lazy val context = Context(self)

  private var _exports: AnyRef = _
  def getExports: AnyRef = _exports
  def setExports(o: AnyRef) {_exports = o}

  private object _require extends JFunction[String, AnyRef] with Require {

    protected def moduleWithObj[T](path: String, t: Option[Class[T]]): WithObj[T] = _resolve(path)(_dir) map {
      case (true, resolved) => Option(_core.get(resolved)) map {WithObj[T](_, None)} getOrElse
          _coreModule(resolved, t)
      case (false, resolved) => Option(_cache.get(resolved)) map {WithObj[T](_, None)} getOrElse
          _loadModule(resolved, t)
    } getOrElse {
      throw new RuntimeException(s"Error: Cannot find module '$path'")
    }

    def apply(path: String): AnyRef = moduleWithObj(path, None).module.getExports

    def impl[T <: Any](path: String, t: Class[T]): T = _impl(resolve(path), t).asInstanceOf[T]
    private val _impl = Memo2[String, Class[_], Any](impl0)

    private def impl0(path: String, t: Class[_]): Any = moduleWithObj(path, t).obj getOrElse
        {throw new RuntimeException(s"Error: Cannot implement type ${t.getName} with $path")}

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
      } else if (isCore(path)) (true, path) else inClasspath(path) orElse inNodeModules(path)(dir)
    }

    private def resolveDir(dir: File): Option[(Boolean, String)] = {
      val main = Try {
        val opt = JSON.parseFull(Source.fromFile(new File(dir, "package.json")).mkString)
        opt.get.asInstanceOf[Map[String, String]]("main")
      }.toOption getOrElse "./index.js"
      _resolve(main)(dir)
    }

    private def inClasspath(path: String): Option[(Boolean, String)] =
      List("", ".js", ".json") map {path + _} collectFirst {
        case p if getClass.getClassLoader.getResource(p) != null => false -> p
      }

    private def inNodeModules(path: String)(dir: File): Option[(Boolean, String)] = {
      if (dir == null) None
      else _resolve(new File(new File(dir, "node_modules"), path).getPath)(dir) match {
        case s@Some(_) => s
        case None => inNodeModules(path)(dir.getParentFile)
      }
    }

    def getCache: JMap[String, Module] = _cache // global, map: id -> module

    private def source(resolved: String): BufferedSource =
      if (resolved.startsWith(".") || resolved.startsWith("/")) Source.fromFile(resolved)
      else Source.fromInputStream(getClass.getClassLoader.getResourceAsStream(resolved))

    private def _loadModule[T](resolved: String, t: Option[Class[T]]): WithObj[T] = {
      val module = new Module(resolved, self)
      _cache.put(resolved, module)

      val Ext = """.*(\.\w+)$""".r
      val obj: Option[T] = resolved match {
        case Ext(".json") =>
          module._exports = (Try {
            import scala.collection.JavaConversions._
            JSON.parseFull(source(resolved).mkString).get match {
              case a: Map[String, AnyRef] => a: JMap[String, AnyRef]
              case a: List[AnyRef] => a: JList[AnyRef]
            }
          } recover {
            case e => throw new RuntimeException(s"JSON parse error: $resolved", e)
          }).get
          None
        case Ext(".js") | _ =>
          module.context.eval(source(resolved).bufferedReader(), t)
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
      val module = new Module(resolved, root)
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
