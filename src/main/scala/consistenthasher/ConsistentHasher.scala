package consistenthasher

import cats.implicits._
import scala.util.hashing.MurmurHash3.stringHash
import scala.util.Random
import scala.collection.immutable.ArraySeq
import scala.collection.SortedMap

case class Bucket(id: Int, angle: Int, data: Map[String, String] = Map.empty) {

  def insert(key: String, value: String): Bucket =
    copy(data=this.data + (key -> value))

  def remove(key: String): Bucket =
    copy(data=this.data - key)
}

case class ConsistentHasher(
  private val buckets: ArraySeq[Bucket],
  private val angleToIndex: SortedMap[Int, Int],
) {

  private lazy val boundedSearch =
    new BoundedSearch(this.angleToIndex.keys.toIndexedSeq, 0, 360)

  import ConsistentHasher.getDegrees

  private def getBucketAngle(key: String): Int =
    getDegrees(Math.abs(stringHash(key)))

  private def getBucket(key: String): Bucket = {
    val keyAngle = getBucketAngle(key)
    val targetAngle = boundedSearch.findNext(keyAngle)
    val targetIndex = this.angleToIndex(targetAngle)
    this.buckets(targetIndex)
  }

  def add(key: String, value: String): ConsistentHasher = {
    val bucket = getBucket(key)
    val newBuckets =
      this.buckets.updated(bucket.id, bucket.insert(key, value))
    copy(buckets=newBuckets)
  }

  def remove(key: String): ConsistentHasher = {
    val bucket = getBucket(key)
    val newBuckets = this.buckets.updated(bucket.id, bucket.remove(key))
    copy(buckets=newBuckets)
  }

  def addNode: ConsistentHasher = {
    val newNodePosition = getDegrees(Random.between(0, Int.MaxValue))
    val newNodeId = this.buckets.size

    val rebalancingTarget = boundedSearch.findNext(newNodePosition)
    val targetIndex = this.angleToIndex(rebalancingTarget)
    val (newBucket, oldBucket) =
      this.buckets(targetIndex)
        .data
        .partition({
          case (key, _) => getBucketAngle(key) <= newNodePosition
        })

    val newBuckets =
      this.buckets
        .updated(targetIndex, this.buckets(targetIndex).copy(data=oldBucket))
        .appended(Bucket(newNodeId, newNodePosition, newBucket))
    val newBucketPositions = this.angleToIndex ++ Seq((newNodePosition -> newNodeId))
    ConsistentHasher(newBuckets, newBucketPositions)
  }

  override def toString(): String =
    this.buckets
      .zipWithIndex
      .map(_.swap)
      .toMap
      .toString
}

object ConsistentHasher {

  private def getDegrees(hash: Int): Int =
    ((hash.toLong * 360L) / Int.MaxValue.toLong).toInt

  def apply(bucketsNumber: Int): ConsistentHasher = {
    val buckets =
      (0 until bucketsNumber)
        .map(Bucket(_, getDegrees(Random.between(0, Int.MaxValue))))
        .to(ArraySeq)
    val bucketPositions =
      buckets
        .map(bucket => (bucket.angle -> bucket.id))
        .to(SortedMap)
    ConsistentHasher(buckets, bucketPositions)
  }
}
