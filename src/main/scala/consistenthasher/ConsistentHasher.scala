package consistenthasher

import cats.implicits._
import scala.util.hashing.MurmurHash3.stringHash
import scala.util.Random
import scala.collection.immutable.ArraySeq
import scala.collection.SortedMap

case class Bucket(id: Int, angle: Int, data: Map[String, String] = Map.empty)

case class ConsistentHasher(
  private val buckets: ArraySeq[Bucket] = ArraySeq(),
  private val angleToIndex: SortedMap[Int, Int] = SortedMap(),
) {

  import ConsistentHasher.getDegrees

  private def getBucketAngle(key: String): Int =
    getDegrees(Math.abs(stringHash(key)))

  private def getBucket(key: String): Bucket = {
    val bucketsAngles = this.angleToIndex.keys.toIndexedSeq
    val keyAngle = getBucketAngle(key)
    val boundedSearch = new BoundedSearch(bucketsAngles, 0, 360)
    val targetAngle = boundedSearch.findNext(keyAngle)
    val targetIndex = this.angleToIndex(targetAngle)
    this.buckets(targetIndex)
  }

  def add(key: String, value: String): ConsistentHasher = {
    val bucket = getBucket(key)
    val newBuckets =
      this.buckets.updated(bucket.id, bucket.copy(data=bucket.data + (key -> value)))
    this.copy(buckets=newBuckets)
  }

  def remove(key: String): ConsistentHasher = {
    val bucket = getBucket(key)
    val newBuckets = this.buckets.updated(bucket.id, bucket.copy(data=bucket.data - key))
    this.copy(buckets=newBuckets)
  }

  def addNode: ConsistentHasher = {
    val newNodePosition = getDegrees(Random.between(0, Int.MaxValue))
    val newNodeId = this.buckets.size

    val bucketsAngles = this.angleToIndex.keys.toIndexedSeq
    val boundedSearch = new BoundedSearch(bucketsAngles, 0, 360)
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

  def apply(nodesNb: Int): ConsistentHasher = {
    val nodes =
      (0 until nodesNb)
        .map(i =>
          Bucket(i, getDegrees(Random.between(0, Int.MaxValue)))
        )
        .to(ArraySeq)
    val bucketPositions =
      nodes
        .map(bucket => (bucket.angle -> bucket.id))
        .to(SortedMap)
    ConsistentHasher(nodes, bucketPositions)
  }
}
