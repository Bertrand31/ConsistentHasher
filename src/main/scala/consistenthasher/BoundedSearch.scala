package consistenthasher

import scala.annotation.tailrec
import cats.implicits._

class BoundedSearch(seq: IndexedSeq[Int], lower: Int, upper: Int) {

  require(!seq.isEmpty)
  require(upper >= lower)

  @tailrec
  private def binaryFindNext(pointer: Int, lastBest: Int, target: Int): Int =
    if (pointer >= this.seq.size) this.seq.head
    else
      this.seq(pointer) match {
        case x if x == target                     => x
        case x if x < target && lastBest > target => lastBest
        case x if x < target                      => binaryFindNext(pointer + 1, lastBest, target)
        case x if x > target                      => binaryFindNext(pointer - 1, x, target)
      }

  def findNext(elem: Int): Int = {
    require(elem >= lower && elem <= upper)
    val bestGuess = Math.ceil(this.seq.size * (elem - lower) / (upper - lower)).toInt - 1
    if (bestGuess === -1) binaryFindNext(0, seq.head, elem)
    else binaryFindNext(bestGuess, seq(bestGuess), elem)
  }
}
