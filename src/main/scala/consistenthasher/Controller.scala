package consistenthasher

import cats.effect.IO
import cats.implicits._
import scala.util.hashing.MurmurHash3.stringHash
import scala.util.Random
import scala.collection.immutable.ArraySeq
import scala.collection.SortedMap

class Controller {

  private def getDegrees(hash: Int): Int =
    ((hash.toLong * 360L) / Int.MaxValue.toLong).toInt

  private var buckets = ArraySeq[Map[String, String]]()
  private var bucketPositions = SortedMap[Int, Int]()

  def initialize(nodes: Int): IO[Unit] = IO {
    this.buckets = ArraySeq.fill(nodes)(Map.empty[String, String])
    this.bucketPositions =
      (0 until nodes)
        .map(i =>
          (getDegrees(Random.between(0, Int.MaxValue)) -> i)
        )
        .to(SortedMap)
  }

  private def getPosition(key: String): Int =
    getDegrees(Math.abs(stringHash(key)))

  private def getBucket(key: String): Int = {
    val bucketsAngles = this.bucketPositions.keys.toIndexedSeq
    val keyAngle = getPosition(key)
    val boundedSearch = new BoundedSearch(bucketsAngles, 0, 360)
    boundedSearch.findNext(keyAngle)
  }

  def add(key: String, value: String): IO[Unit] = IO {
    val bucket = getBucket(key)
    val bucketIndex = this.bucketPositions(bucket)
    this.buckets = this.buckets.updated(bucketIndex, this.buckets(bucketIndex) + (key -> value))
  }

  def remove(key: String): IO[Unit] = IO {
    val bucket = getBucket(key)
    this.buckets = this.buckets.updated(bucket, this.buckets(bucket) - key)
  }

  def addNode: IO[Unit] = IO {
    val newNodePosition = getDegrees(Random.between(0, Int.MaxValue))
    val newNodeId = this.bucketPositions.size

    val bucketsAngles = this.bucketPositions.keys.toIndexedSeq
    val boundedSearch = new BoundedSearch(bucketsAngles, 0, 360)
    val rebalancingTarget = boundedSearch.findNext(newNodePosition)
    val targetIndex = bucketPositions(rebalancingTarget)
    val (newBucket, oldBucket) =
      this.buckets(targetIndex)
        .partition({
          case (key, _) => getPosition(key) <= newNodePosition
        })

    this.bucketPositions = this.bucketPositions + (newNodePosition -> newNodeId)
    this.buckets =
      this.buckets
        .updated(targetIndex, oldBucket)
        .appended(newBucket)
  }

  def show: Map[Int, Map[String, String]] =
    this.buckets
      .zipWithIndex
      .map(_.swap)
      .toMap
}
