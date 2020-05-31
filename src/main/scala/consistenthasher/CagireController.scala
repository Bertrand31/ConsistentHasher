package consistenthasher

import cats.effect.IO
import cats.implicits._
import scala.util.hashing.MurmurHash3.stringHash
import scala.util.Random
import scala.collection.immutable.ArraySeq

case class Row(key: String, value: String)

class Controller {

  private def getDegrees(hash: Int): Long =
    (hash.toLong * 360L) / Int.MaxValue.toLong

  private var buckets = ArraySeq[Set[Row]]()
  private var bucketPositions = Map[Long, Int]()

  def initialize(nodes: Int): IO[Unit] = IO {
    this.buckets = ArraySeq.fill(nodes)(Set.empty[Row])
    this.bucketPositions =
      (0 until nodes)
        .map(i =>
          (getDegrees(Random.between(0, Int.MaxValue)) -> i)
        )
        .toMap
  }

  private def getPosition(key: String): Long =
    getDegrees(stringHash(key))

  private def getBucket(key: String): Int =
    this.bucketPositions
      .toList
      .dropWhile(_._1 < getPosition(key))
      .headOption
      .fold(this.bucketPositions.head._2)(_._2)

  def add(key: String, value: String): IO[Unit] = IO {
    val bucket = getBucket(key)
    this.buckets = this.buckets.updated(bucket, this.buckets(bucket) + Row(key, value))
  }

  def remove(key: String): IO[Unit] = IO {
    val bucket = getBucket(key)
    this.buckets = this.buckets.updated(bucket, this.buckets(bucket).filterNot(_.key === key))
  }

  def show: Map[Int, Set[Row]] =
    this.buckets
      .zipWithIndex
      .map(_.swap)
      .toMap
}
