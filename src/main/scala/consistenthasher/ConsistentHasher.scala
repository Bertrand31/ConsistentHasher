package consistenthasher

import scala.util.hashing.MurmurHash3.stringHash
import scala.collection.SortedMap
import scala.collection.immutable.ArraySeq
import cats.implicits._
import utils.Seed
import utils.ArraySeqUtils.AugmentedArraySeq

final case class ConsistentHasher[A, B](
  private val buckets: ArraySeq[Bucket[A, B]],
  private val angleToIndex: SortedMap[Float, Int],
  private val randomState: Seed,
) {

  private lazy val findNext =
    BoundedSearch.findNext[Float](this.angleToIndex.keys.toIndexedSeq, 0, 360)

  import ConsistentHasher.getDegrees

  private def getBucketAngle(key: A): Float =
    getDegrees(Math.abs(stringHash(key.toString)))

  private def getBucketId(key: A): Int = {
    val keyAngle = getBucketAngle(key)
    val targetAngle = findNext(keyAngle)
    this.angleToIndex(targetAngle)
  }

  def add(key: A, value: B): ConsistentHasher[A, B] = {
    val bucketId = getBucketId(key)
    val newBuckets = this.buckets.updatedWith(bucketId, _.insert(key, value))
    copy(buckets=newBuckets)
  }

  def remove(key: A): ConsistentHasher[A, B] = {
    val bucketId = getBucketId(key)
    val newBuckets = this.buckets.updatedWith(bucketId, _.remove(key))
    copy(buckets=newBuckets)
  }

  def addNode: ConsistentHasher[A, B] = {
    val (rand, nextSeed) = this.randomState.gen
    val newNodePosition = getDegrees(rand.toInt)
    val newNodeId = this.buckets.size

    val rebalancingTarget = findNext(newNodePosition)
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

  def showBuckets: Map[Int, Map[A, B]] =
    this.buckets
      .zipWithIndex
      .map({ case (bucket, index) => (index, bucket.data) })
      .toMap
}

object ConsistentHasher {

  import scala.util.Random

  private def getDegrees(hash: Int): Float =
    ((hash.toLong * 360) / Long.MaxValue.toDouble).toFloat

  def apply[A, B](bucketsNumber: Int, randSeed: Long = Random.nextLong): ConsistentHasher[A, B] = {
    val baseSeed = Seed(randSeed)
    val ((_, nextSeed) +: seeds) =
      (0 until bucketsNumber)
        .foldLeft(List(baseSeed.gen))((acc, _) => acc.head._2.gen +: acc)
    val buckets =
      seeds
        .map(_._1.toInt)
        .map(getDegrees)
        .zipWithIndex
        .map({ case (angle, id) => Bucket(id, angle, Map.empty[A, B]) })
        .to(ArraySeq)
    val bucketPositions =
      buckets
        .map(bucket => (bucket.angle -> bucket.id))
        .to(SortedMap)
    ConsistentHasher[A, B](buckets, bucketPositions, nextSeed)
  }
}
