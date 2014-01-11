package org.killingbilling.junction

import java.io.ByteArrayOutputStream
import java.lang.{Double => JDouble}
import java.nio.charset.Charset
import java.nio.file.Paths
import java.util.{List => JList, Map => JMap}
import org.killingbilling.junction.utils._
import org.scalatest.{Matchers, FreeSpec}
import scala.collection.JavaConversions._

object ModuleSpec {

  implicit val js = newEngine()

  assert(js != null, "ENGINE SHOULD NOT BE NULL")

  val rootModule = new Module()
  val require = rootModule.getRequire

  val workDir = Paths.get(".").toAbsolutePath.normalize()

  def resolvedPath(s: String) = workDir.resolve(s).normalize().toString

  def jsObject(o: Map[String, AnyRef]): JMap[String, AnyRef] = o
  def jsArray(o: List[AnyRef]): JList[AnyRef] = o

  val out = new ByteArrayOutputStream()
  val err = new ByteArrayOutputStream()
  Console.setOut(out)
  Console.setErr(err)
}

class ModuleSpec extends FreeSpec with Matchers {

  import ModuleSpec._

  "require.resolve(): " in {
    require.resolve("lib/dummy") shouldBe resolvedPath("./node_modules/lib/dummy.js")
    require.resolve("./src/test/js/dumb.js") shouldBe resolvedPath("./src/test/js/dumb.js.js")
    require.resolve("./src/test/js/d") shouldBe resolvedPath("./src/test/js/d/lib/main.js")
    require.resolve("./src/test/js/d.js") shouldBe resolvedPath("./src/test/js/d.js/index.js")
  }

  "require(): " in {
    require("./src/test/js/dummy.txt") shouldBe jsObject(Map("dummyID" -> "dummy"))
    require("./src/test/js/someObj.json") shouldBe jsObject(Map("qq" -> "QQ", "n" -> (2.0: JDouble)))
    require("./src/test/js/someArr.json") shouldBe jsArray(List(4.0: JDouble, "abra", "cada", 2.0: JDouble, "bra"))
    require("./src/test/js/d") shouldBe "(arg: QQ)"
    require("./src/test/js/d.js") shouldBe "(arg: QQ.js)"
  }

  "require() cycle" in {
    val expectedOutput =
      """
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
    out.toString(Charset.defaultCharset().name()).trim shouldBe expectedOutput.trim
  }

  "process: " in {
    val p = require("./src/test/js/process.js").asInstanceOf[JMap[String, AnyRef]].toMap
    p("noDeprecation") shouldBe false
    p("throwDeprecation") shouldBe true
    p("traceDeprecation") shouldBe true
  }

  "process.stdout: " in {
    out.reset()
    require("./src/test/js/writeHello.js")
    out.toString(Charset.defaultCharset().name()) shouldBe "HELLO!"
  }

  "load lib/console.js" in {
    getClass.getClassLoader.getResource("lib/console.js") shouldNot be(null)
  }

  "console.log" in {
    out.reset()
    require("./src/test/js/logHello.js")
    out.toString(Charset.defaultCharset().name()) shouldBe "LOGGING HELLO!\n"
  }

  "Buffer: " in {
    val a = require("./src/test/js/ass.js").asInstanceOf[JMap[String, AnyRef]].toMap
    a("isBuffer") shouldBe false
  }

}
