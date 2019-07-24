package test.utest.examples

import utest._

object TestPathTests extends TestSuite {
  def check(item: String)(implicit testPath: utest.framework.TestPath) = {
    println(testPath.value.last)
  }
  val tests = Tests {
    test("lihaoyi/fastparse") {
      check("2")
    }
  }
}
