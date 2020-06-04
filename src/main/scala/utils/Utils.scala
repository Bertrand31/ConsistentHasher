package utils

import scala.collection.immutable.ArraySeq

object ArraySeqUtils {

  implicit class AugmentedArraySeq[A](val arr: ArraySeq[A]) {

    def updatedWith(index: Int, fn: A => A): ArraySeq[A] =
      arr.updated(index, fn(arr(index)))

    def removeAt(index: Int): ArraySeq[A] =
      arr.take(index) ++ arr.drop(index + 1)
  }
}
