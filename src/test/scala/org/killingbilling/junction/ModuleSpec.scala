package org.killingbilling.junction

import org.killingbilling.junction.utils._
import org.scalatest.{Matchers, FreeSpec}
import java.io.File
import java.nio.file.Paths
import java.util
import scala.collection.JavaConversions._

object ModuleSpec {

  implicit val js = newEngine()

  val rootModule = new Module()
  val require = rootModule._require

  val workDir = Paths.get(".").toAbsolutePath.normalize()

  def resolvedPath(s: String) = workDir.resolve(s).normalize().toString

}

class ModuleSpec extends FreeSpec with Matchers {

  import ModuleSpec._

  "require.resolve(): " in {
    require.resolve("lib/dummy") shouldBe resolvedPath("node_modules/lib/dummy.js")
  }

  "require(): " in {
    require("./src/test/js/dummy.txt") should equal (Map("dummyID" -> "dummy"): java.util.Map[String, Object])
    //require("./src/test/js/some.json") should equal (Map("qq" -> "QQ"): java.util.Map[String, Object])
  }

}
