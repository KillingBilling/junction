package org.killingbilling.junction

import org.killingbilling.junction.utils._
import org.scalatest.{FreeSpec, Matchers}

class ScriptEngineSpec extends FreeSpec with Matchers {

  "engine should not be null" in {
    newEngine() should not be null
  }

}
