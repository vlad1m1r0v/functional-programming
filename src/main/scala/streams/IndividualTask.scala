package streams

import scala.annotation.tailrec

object IndividualTask {
  val calcExpression: PartialFunction[(Int, Int), Long] = new PartialFunction[(Int, Int), Long] {

    def apply(x: (Int, Int)): Long = {

      @tailrec
      def calcProduct(x: (Int, Int), result: Long = x._1.toLong): Long = {
        if (x._2 == 1) result
        else calcProduct((x._1, x._2 - 1), result * x._2 * x._1)
      }

      if (x._1 < x._2) x._2 else calcProduct((x._1, 15))
    }

    override def isDefinedAt(x: (Int, Int)): Boolean = x._1 != x._2
  }

  def toList(range: Seq[Int], f: PartialFunction[(Int, Int), Long]): List[Long] = range.collect(x => f(x, 5)).toList
}

