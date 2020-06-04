package consistenthasher

import scala.annotation.tailrec
import cats.implicits._

object BoundedSearch {

  def findNext(seq: IndexedSeq[Int], lower: Int, upper: Int)(elem: Int): Int = {
    require(!seq.isEmpty)
    require(upper >= lower)
    require(elem >= lower && elem <= upper)
    val bestGuess = Math.ceil(seq.size * (elem - lower) / (upper - lower)).toInt - 1

    @tailrec
    def binaryFindNext(pointer: Int, lastBest: Int, target: Int): Int =
      if (pointer >= seq.size) seq.head
      else
        seq(pointer) match {
          case x if x === target                    => x
          case x if x < target && lastBest > target => lastBest
          case x if x < target                      => binaryFindNext(pointer + 1, lastBest, target)
          case x if x > target && pointer === 0     => x
          case x if x > target                      => binaryFindNext(pointer - 1, x, target)
        }
    if (bestGuess === -1) binaryFindNext(0, seq.head, elem)
    else binaryFindNext(bestGuess, seq(bestGuess), elem)
  }
}
