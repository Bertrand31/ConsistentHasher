import org.scalatest.flatspec.AnyFlatSpec
import consistenthasher.BoundedSearch

class BoundedSearchSpec extends AnyFlatSpec {

  behavior of "the BoundedSearch utility"

  behavior of "the findNext function"

  it should "return the next biggest number" in {

    val seq = Vector(3, 5, 8, 9, 10, 14, 19, 22)
    val lowerBound = 3
    val upperBound = 30
    val boundedSearch = new BoundedSearch(seq, lowerBound, upperBound)
    assert(boundedSearch.findNext(13) == 14)
    assert(boundedSearch.findNext(4) == 5)
    assert(boundedSearch.findNext(14) == 14)
    assert(boundedSearch.findNext(23) == 3)
    assert(boundedSearch.findNext(3) == 3)
  }
}
