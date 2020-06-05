package consistenthasher

import scala.annotation.tailrec
import cats.implicits._
import cats.kernel.{Eq => CatsEq}

object BoundedSearch {

  def findNext[T](seq: IndexedSeq[T], lower: T, upper: T)(implicit num: Fractional[T]): T => T = {
    import num._
    implicit val eqNumeric = CatsEq.fromUniversalEquals[T]
    require(!seq.isEmpty, "Passed an empty sequence")
    require(upper >= lower, "The upper bound cannot be lower than the lower bound")

    @tailrec
    def binaryFindNext(pointer: Int, lastBest: T, target: T): T =
      if (pointer >= seq.size) seq.head
      else
        seq(pointer) match {
          case x if x === target                    => x
          case x if x < target && lastBest > target => lastBest
          case x if x < target                      => binaryFindNext(pointer + 1, lastBest, target)
          case x if x > target && pointer === 0     => x
          case x if x > target                      => binaryFindNext(pointer - 1, x, target)
        }

    (elem: T) => {
      require(elem >= lower && elem <= upper, "Given element is not within bounds")
      val bestGuess = (seq.size - 1) * (elem - lower).toFloat / (upper - lower).toFloat
      val roundedGuess = Math.ceil(bestGuess).toInt
      binaryFindNext(roundedGuess, seq(roundedGuess), elem)
    }
  }
}
