package consistenthasher

import cats.effect.IO
import cats.implicits._
import scala.util.hashing.MurmurHash3.stringHash
import scala.util.Random
import scala.collection.immutable.ArraySeq
import scala.collection.SortedMap

class Controller {

  private def getDegrees(hash: Int): Long =
    (hash.toLong * 360L) / Int.MaxValue.toLong

  private var buckets = ArraySeq[Map[String, String]]()
  private var bucketPositions = SortedMap[Long, Int]()

  def initialize(nodes: Int): IO[Unit] = IO {
    this.buckets = ArraySeq.fill(nodes)(Map.empty[String, String])
    this.bucketPositions =
      (0 until nodes)
        .map(i =>
          (getDegrees(Random.between(0, Int.MaxValue)) -> i)
        )
        .to(SortedMap)
  }

  private def getPosition(key: String): Long =
    getDegrees(Math.abs(stringHash(key)))

  private def getBucket(key: String): Int =
    this.bucketPositions
      .toList
      .dropWhile(_._1 < getPosition(key))
      .headOption
      .fold(this.bucketPositions.head._2)(_._2)

  def add(key: String, value: String): IO[Unit] = IO {
    val bucket = getBucket(key)
    this.buckets = this.buckets.updated(bucket, this.buckets(bucket) + (key -> value))
  }

  def remove(key: String): IO[Unit] = IO {
    val bucket = getBucket(key)
    this.buckets = this.buckets.updated(bucket, this.buckets(bucket) - key)
  }

  // def addNode: IO[Unit] = IO {
    // val newNodePosition = getDegrees(Random.between(0, Int.MaxValue))
    // val newNodeId = this.bucketPositions.size
    // this.bucketPositions = this.bucketPositions + (newNodePosition -> newNodeId)
  // }

  def show: Map[Int, Map[String, String]] =
    this.buckets
      .zipWithIndex
      .map(_.swap)
      .toMap
}
