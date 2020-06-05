package consistenthasher

import scala.annotation.tailrec
import cats.implicits._

object BoundedSearch {

  def findNext(seq: IndexedSeq[Float], lower: Float, upper: Float): Float => Float = {
    require(!seq.isEmpty)
    require(upper >= lower)

    @tailrec
    def binaryFindNext(pointer: Int, lastBest: Float, target: Float): Float =
      if (pointer >= seq.size) seq.head
      else
        seq(pointer) match {
          case x if x === target                    => x
          case x if x < target && lastBest > target => lastBest
          case x if x < target                      => binaryFindNext(pointer + 1, lastBest, target)
          case x if x > target && pointer === 0     => x
          case x if x > target                      => binaryFindNext(pointer - 1, x, target)
        }

    (elem: Float) => {
      require(elem >= lower && elem <= upper)
      val bestGuess = Math.ceil((seq.size - 1) * (elem - lower).toDouble / (upper - lower)).toInt
      binaryFindNext(bestGuess, seq(bestGuess), elem)
    }
  }
}
