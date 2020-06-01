package consistenthasher

import scala.util.hashing.MurmurHash3.stringHash
import scala.collection.SortedMap
import scala.collection.immutable.ArraySeq
import cats.implicits._
import utils.Seed

case class Bucket(id: Int, angle: Int, data: Map[String, String] = Map.empty) {

  def insert(key: String, value: String): Bucket =
    copy(data=this.data + (key -> value))

  def remove(key: String): Bucket =
    copy(data=this.data - key)
}

case class ConsistentHasher(
  private val buckets: ArraySeq[Bucket],
  private val angleToIndex: SortedMap[Int, Int],
  private val randomState: Seed,
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
    val newNodePosition = getDegrees(this.randomState.long)
    val newNodeId = this.buckets.size

    val rebalancingTarget = boundedSearch.findNext(newNodePosition)
    val targetIndex = this.angleToIndex(rebalancingTarget)
    val targetBucket = this.buckets(targetIndex)
    val (newBucket, oldBucket) =
      targetBucket
        .data
        .partition({
          case (key, _) => getBucketAngle(key) <= newNodePosition
        })

    val newBuckets =
      this.buckets
        .updated(targetIndex, targetBucket.copy(data=oldBucket))
        .appended(Bucket(newNodeId, newNodePosition, newBucket))
    val newBucketPositions = this.angleToIndex ++ Seq((newNodePosition -> newNodeId))

    ConsistentHasher(
      buckets=newBuckets,
      angleToIndex=newBucketPositions,
      randomState=this.randomState.next,
    )
  }

  override def toString(): String =
    this.buckets
      .zipWithIndex
      .map(_.swap)
      .toMap
      .toString
}

object ConsistentHasher {

  import scala.util.Random

  private def getDegrees(hash: Long): Int =
    ((hash.toLong * 360L) / Int.MaxValue.toLong).toInt

  def apply(bucketsNumber: Int): ConsistentHasher = {
    val randSeed = Seed(Random.nextInt)
    val buckets =
      (0 until bucketsNumber)
        .map(Bucket(_, getDegrees(randSeed.long))) // FIXME
        .to(ArraySeq)
    val bucketPositions =
      buckets
        .map(bucket => (bucket.angle -> bucket.id))
        .to(SortedMap)
    ConsistentHasher(buckets, bucketPositions, randSeed)
  }
}
