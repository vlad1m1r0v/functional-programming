package quickcheck

import org.scalacheck.Prop.{forAll, propBoolean}
import org.scalacheck.{Gen, Properties, Arbitrary}

import scala.annotation.tailrec

trait IndividualTask {
  // x < k and k >= 1
  lazy val myGen1: Gen[(Int, Int)] = for {
    k <- Gen.choose(Short.MinValue, Short.MaxValue).suchThat(_ >= 1)
    x <- Gen.choose(Short.MinValue, 2 * Short.MaxValue).suchThat(_ < k)
  } yield (x, k)

  // x > k and k >= 1
  lazy val myGen2: Gen[(Int, Int)] = for {
    k <- Gen.choose(1, 10)
    x <- Gen.choose(1, 100).suchThat(_ > k)
  } yield (x, k)

  // x = k or k < 1
  lazy val myGen3: Gen[(Int, Int)] = for {
    x <- Gen.choose(Short.MinValue, Short.MaxValue)
    k <- Gen.oneOf(Gen.const(x), Gen.choose(Short.MinValue, 0.toShort))
  } yield (x, k)

  @tailrec
  final def calcProduct(x: (Int, Int), result: Long): Long = {
    if (x._2 == 1) result
    else
      calcProduct((x._1, x._2 - 1), result * x._2 * x._1)
  }

  val calcExpression: PartialFunction[(Int, Int), Long] = new PartialFunction[(Int, Int), Long] {

    def apply(x: (Int, Int)): Long = {

      if (x._1 < x._2) x._2 else calcProduct((x._1, 15), 1)
    }

    override def isDefinedAt(x: (Int, Int)): Boolean = x._1 != x._2 && x._2 >= 1
  }

  val liftedCE: ((Int, Int)) => Option[Long] = calcExpression.lift
}

object QuickCheckIndividualTask extends Properties("IndividualTask") with IndividualTask {
  property("if x < k and k >= 1 then should return k") = forAll(myGen1)(x => liftedCE(x).contains(x._2))

  property("if x > k and k >= 1 then should return calcProduct") = forAll(myGen2)(x => {
    liftedCE(x).contains(calcProduct((x._1, 15), 1))
  })

  property("if x = k or k < 1 then should return None") = forAll(myGen3) { (x: (Int, Int)) =>
    liftedCE(x).isEmpty
  }
}
