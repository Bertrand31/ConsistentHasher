package consistenthasher

import scala.util.hashing.MurmurHash3.stringHash
import scala.collection.SortedMap
import scala.collection.immutable.ArraySeq
import cats.implicits._
import utils.Seed
import utils.ArraySeqUtils.AugmentedArraySeq

final case class ConsistentHasher(
  private val buckets: ArraySeq[Bucket],
  private val angleToIndex: SortedMap[Int, Int],
  private val randomState: Seed,
) {

  private lazy val boundedSearch =
    new BoundedSearch(this.angleToIndex.keys.toIndexedSeq, 0, 360)

  import ConsistentHasher.getDegrees

  private def getBucketAngle(key: String): Int =
    getDegrees(Math.abs(stringHash(key)))

  private def getBucketId(key: String): Int = {
    val keyAngle = getBucketAngle(key)
    val targetAngle = boundedSearch.findNext(keyAngle)
    this.angleToIndex(targetAngle)
  }

  def add(key: String, value: String): ConsistentHasher = {
    val bucketId = getBucketId(key)
    val newBuckets = this.buckets.updatedWith(bucketId, _.insert(key, value))
    copy(buckets=newBuckets)
  }

  def remove(key: String): ConsistentHasher = {
    val bucketId = getBucketId(key)
    val newBuckets = this.buckets.updatedWith(bucketId, _.remove(key))
    copy(buckets=newBuckets)
  }

  def addNode: ConsistentHasher = {
    val (rand, nextSeed) = this.randomState.gen
    val newNodePosition = getDegrees(rand)
    val newNodeId = this.buckets.size

    val rebalancingTarget = boundedSearch.findNext(newNodePosition)
    val targetIndex = this.angleToIndex(rebalancingTarget)
    val targetBucket = this.buckets(targetIndex)
    val (newBucketData, oldBucketData) =
      targetBucket
        .data
        .partition({
          case (key, _) => getBucketAngle(key) <= newNodePosition
        })

    val newBuckets =
      this.buckets
        .updated(targetIndex, targetBucket.copy(data=oldBucketData))
        .appended(Bucket(newNodeId, newNodePosition, newBucketData))
    val newBucketPositions = this.angleToIndex ++ Seq((newNodePosition -> newNodeId))

    ConsistentHasher(
      buckets=newBuckets,
      angleToIndex=newBucketPositions,
      randomState=nextSeed,
    )
  }

  def showBuckets(): Map[Int, Map[String, String]] =
    this.buckets
      .zipWithIndex
      .map({ case (bucket, index) => (index, bucket.data) })
      .toMap
}

object ConsistentHasher {

  import scala.util.Random

  private def getDegrees(hash: Long): Int =
    ((hash.toDouble * 360D) / Long.MaxValue.toDouble).toInt

  def apply(bucketsNumber: Int, randSeed: Long = Random.nextLong): ConsistentHasher = {
    val baseSeed = Seed(randSeed)
    val ((_, nextSeed) +: seeds) =
      (0 until bucketsNumber)
        .foldLeft(List(baseSeed.gen))((acc, _) => acc.head._2.gen +: acc)
    val buckets =
      seeds
        .map(_._1)
        .map(getDegrees)
        .zipWithIndex
        .map({ case (angle, id) => Bucket(id, angle) })
        .to(ArraySeq)
    val bucketPositions =
      buckets
        .map(bucket => (bucket.angle -> bucket.id))
        .to(SortedMap)
    ConsistentHasher(buckets, bucketPositions, nextSeed)
  }
}
