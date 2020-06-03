package utils

import scala.collection.immutable.ArraySeq

object ArraySeqUtils {

implicit class AugmentedArraySeq[A](val arr: ArraySeq[A]) {

    def updatedWith(index: Int, fn: A => A): ArraySeq[A] =
      arr.updated(index, fn(arr(index)))

    def insertAt(index: Int, elem: A): ArraySeq[A] = {
      val (front, back) = arr.splitAt(index)
      (front :+ elem) ++ back
    }

    def removeAt(index: Int): ArraySeq[A] =
      arr.take(index) ++ arr.drop(index + 1)
  }
}
