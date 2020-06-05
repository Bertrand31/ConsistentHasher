import org.scalatest.flatspec.AnyFlatSpec
import consistenthasher.BoundedSearch

class BoundedSearchSpec extends AnyFlatSpec {

  behavior of "the BoundedSearch utility"

  behavior of "the findNext function"

  it should "return the next biggest number" in {

    val seq = Vector[Float](3, 5, 8, 9, 10, 14, 19, 22)
    val lowerBound = 3F
    val upperBound = 30F
    val boundedSearch = BoundedSearch.findNext[Float](seq, lowerBound, upperBound)
    assert(boundedSearch(13) === 14)
    assert(boundedSearch(4) === 5)
    assert(boundedSearch(14) === 14)
    assert(boundedSearch(23) === 3)
    assert(boundedSearch(3) === 3)
  }

  it should "return the next biggest number in an edge case" in {

    val seq = Vector[Float](3, 5, 8, 9, 10, 14, 19, 22)
    val lowerBound = 0F
    val upperBound = 30F
    val boundedSearch = BoundedSearch.findNext[Float](seq, lowerBound, upperBound)
    assert(boundedSearch(1) === 3)
  }
}
