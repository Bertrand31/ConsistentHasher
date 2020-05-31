package consistenthasher

import cats.effect.IO
import cats.implicits._
import scala.util.hashing.MurmurHash3.stringHash
import scala.util.Random
import scala.collection.immutable.ArraySeq

class Controller {

  private def getDegrees(hash: Int): Long =
    (hash * 360L) / Int.MaxValue

  case class Row(key: String, value: String)

  private var buckets = ArraySeq[List[Row]]()
  private var bucketPositions = Map[Long, Int]()

  def initialize(nodes: Int): IO[Unit] = IO {
    this.buckets = ArraySeq.fill(nodes)(List.empty[Row])
    this.bucketPositions =
      (0 until nodes)
        .map(i =>
          (getDegrees(Random.between(0, Int.MaxValue)) -> i)
        )
        .toMap
  }

  def getPosition(key: String): Long =
    getDegrees(stringHash(key))

  def getBucket(key: String): Int = {
    val position = getPosition(key)
    this.bucketPositions
      .toList
      .dropWhile(_._1 < position)
      .headOption
      .fold(this.bucketPositions.head._2)(_._2)
  }

  def add(key: String, value: String): IO[Unit] = IO {
    val bucket = getBucket(key)
    this.buckets = this.buckets.updated(bucket, Row(key, value) +: this.buckets(bucket))
  }

  def show: Map[Int, List[Row]] =
    this.buckets
      .zipWithIndex
      .map(_.swap)
      .toMap
}
