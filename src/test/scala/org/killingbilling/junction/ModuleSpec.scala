package org.killingbilling.junction

import java.io.ByteArrayOutputStream
import java.lang.{Double => JDouble}
import java.nio.file.Paths
import java.util.{List => JList, Map => JMap}
import javax.script.{ScriptContext, Invocable}
import org.killingbilling.junction.utils._
import org.scalatest.{Matchers, FreeSpec}
import scala.collection.JavaConversions._
import scala.compat.Platform

object ModuleSpec {

  val workDir = Paths.get(".").toAbsolutePath.normalize()
  def resolvedPath(s: String) = workDir.resolve(s).normalize().toString

  def jsObject(o: Map[String, AnyRef]): JMap[String, AnyRef] = o
  def jsArray(o: List[AnyRef]): JList[AnyRef] = o

  val out = new ByteArrayOutputStream()
  val err = new ByteArrayOutputStream()
  Console.setOut(out)
  Console.setErr(err)

  trait ServiceAccount {
    def aggr(a: Double, b: Double): Double
    def init(v: Double): Double
  }

}

class ModuleSpec extends FreeSpec with Matchers {

  import ModuleSpec._

  "require.resolve(): " in {
    val require = Require()
    require.resolve("lib/dummy") shouldBe resolvedPath("./node_modules/lib/dummy.js")
    require.resolve("./src/test/js/dumb.js") shouldBe resolvedPath("./src/test/js/dumb.js.js")
    require.resolve("./src/test/js/d") shouldBe resolvedPath("./src/test/js/d/lib/main.js")
    require.resolve("./src/test/js/d.js") shouldBe resolvedPath("./src/test/js/d.js/index.js")
  }

  "require(): " in {
    val require = Require()
    require("./src/test/js/dummy.txt") shouldBe jsObject(Map("dummyID" -> "dummy"))
    require("./src/test/js/someObj.json") shouldBe jsObject(Map("qq" -> "QQ", "n" -> (2.0: JDouble)))
    require("./src/test/js/someArr.json") shouldBe jsArray(List(4.0: JDouble, "abra", "cada", 2.0: JDouble, "bra"))
    require("./src/test/js/d") shouldBe "(arg: QQ)"
    require("./src/test/js/d.js") shouldBe "(arg: QQ.js)"
  }

  "require() cycle" in {
    val require = Require()
    val expectedOutput = """
                           |main starting
                           |a starting
                           |b starting
                           |in b, a.done = false
                           |b done
                           |in a, b.done = true
                           |a done
                           |in main, a.done=true, b.done=true
                         """.stripMargin
    out.reset()
    require("./src/test/js/main")
    out.toString(Platform.defaultCharsetName).trim shouldBe expectedOutput.trim
  }

  "process: " in {
    val require = Require()
    val p = require("./src/test/js/process.js").asInstanceOf[JMap[String, AnyRef]].toMap
    p("noDeprecation") shouldBe false
    p("throwDeprecation") shouldBe true
    p("traceDeprecation") shouldBe true
  }

  "process.stdout: " in {
    val require = Require()
    out.reset()
    require("./src/test/js/writeHello.js")
    out.toString(Platform.defaultCharsetName) shouldBe "HELLO!"
  }

  "load lib/console.js" in {
    getClass.getClassLoader.getResource("lib/console.js") shouldNot be(null)
  }

  "console.log" in {
    val require = Require()
    out.reset()
    require("./src/test/js/logHello.js")
    out.toString(Platform.defaultCharsetName) shouldBe "LOGGING HELLO!\n"
  }

  "Buffer: " in {
    val require = Require()
    val a = require("./src/test/js/ass.js").asInstanceOf[JMap[String, AnyRef]].toMap
    a("isBuffer") shouldBe false
  }

  "require.impl()" in {
    val require = Require()
    val acc: ServiceAccount = require.impl("./src/test/js/acc.js", classOf[ServiceAccount])
    val acc2: ServiceAccount = require.impl("./src/test/js/acc2.js", classOf[ServiceAccount])

    acc.aggr(1, 2) shouldBe 3
    acc.init(4) shouldBe 0

    acc2.aggr(1, 2) shouldBe 2
    acc2.init(4) shouldBe 1
  }

  "require.impl() cache" in {
    val require = Require()
    val acc: ServiceAccount = require.impl("./src/test/js/acc.js", classOf[ServiceAccount])
    val acc2: ServiceAccount = require.impl("./src/test/js/acc.js", classOf[ServiceAccount])

    acc2 shouldBe theSameInstanceAs(acc)
  }

  "module.exports - impl interface" in {
    val js = newEngine()
    js.getContext.getBindings(ScriptContext.GLOBAL_SCOPE).put("module", new Module())

    js.eval( """
               | module.exports = {
               |   aggr: function(a, b) {return a + b;},
               |   init: function(v) {return (v == null) ? 0 : v;}
               | };
               | """.stripMargin)

    val locals = js.createBindings()
    js.eval("var __exports = module.exports;", locals)
    val acc = js.asInstanceOf[Invocable].getInterface(locals.get("__exports"), classOf[ServiceAccount])

    acc.aggr(1, 2) shouldBe 3
    acc.init(1) shouldBe 1
  }

}
