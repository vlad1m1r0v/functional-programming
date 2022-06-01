package streams

import streams.IndividualTask.{calcExpression, toList}

import scala.annotation.tailrec

class IndividualTaskSuite extends munit.FunSuite {
  val range = (-25 to 25)
  val testList = toList(range, calcExpression)

  test("check case when x < k") {
    val x = -1
    val k = 1
    val result = calcExpression((x, k))
    assertEquals(result, k.toLong)
    print(result)
  }

  test("check case when x > k") {
    val x = 2
    val k = 1
    val result = calcExpression((x, k))
    val expected = 42849873690624000L

    assertEquals(result, expected)
  }

  test("check case when x = k and collect") {
    val x = 5
    val k = x
    val result = List((x, k)).collect(calcExpression)

    assertEquals(result.isEmpty, true)
  }

  test("check count") {
    val result = testList.count(_ > 10e10)
    assertEquals(result, 11)
  }

  test("check filter") {
    val result = testList.filter(_ > 10e18 / 1.3)
    assertEquals(result, List(7783471400328626176L))
  }

  test("check exists") {
    val result = testList.exists(_ > 10e18 / 1.3)
    assertEquals(result, true)
  }

  test("check partition") {
    val lists = testList.partition(_ > 10e16)
    val result = lists._2.length
    assertEquals(result, 40)
  }
}