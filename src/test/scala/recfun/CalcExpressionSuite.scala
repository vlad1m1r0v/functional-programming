package recfun

import org.scalatest.funsuite.AnyFunSuite

import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CalcExpressionSuite extends AnyFunSuite {

  import Main.calcExpression

  test("calc expression: x = 2, k = 1") {
    assert(calcExpression(2, 1) === 42849873690624000L)
  }

  test("calc expression: x = 1, k = 2") {
    assert(calcExpression(1, 2) === 2)
  }

  test("calc expression: x = 2, k = 2") {
    assertThrows[IllegalArgumentException] {
      calcExpression(2, 2)
    }
  }

}
