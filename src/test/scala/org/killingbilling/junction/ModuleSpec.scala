package org.killingbilling.junction

import java.lang.{Double => JDouble}
import java.nio.file.Paths
import org.killingbilling.junction.utils._
import org.scalatest.{Matchers, FreeSpec}

object ModuleSpec {

  import scala.collection.JavaConversions._

  implicit val js = newEngine()

  val rootModule = new Module()
  val require = rootModule._require

  val workDir = Paths.get(".").toAbsolutePath.normalize()

  def resolvedPath(s: String) = workDir.resolve(s).normalize().toString

  def jsObject(o: Map[String, AnyRef]): java.util.Map[String, AnyRef] = o: java.util.Map[String, AnyRef]
  def jsArray(o: List[AnyRef]): java.util.List[AnyRef] = o: java.util.List[AnyRef]

}

class ModuleSpec extends FreeSpec with Matchers {

  import ModuleSpec._

  "require.resolve(): " in {
    require.resolve("lib/dummy") shouldBe resolvedPath("node_modules/lib/dummy.js")
  }

  "require(): " in {
    require("./src/test/js/dummy.txt") shouldBe jsObject(Map("dummyID" -> "dummy"))
    require("./src/test/js/someObj.json") shouldBe jsObject(Map("qq" -> "QQ", "n" -> (2.0: JDouble)))
    require("./src/test/js/someArr.json") shouldBe jsArray(List(4.0: JDouble, "abra", "cada", 2.0: JDouble, "bra"))
  }

}
