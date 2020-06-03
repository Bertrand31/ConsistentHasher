import org.scalatest.flatspec.AnyFlatSpec
import consistenthasher.ConsistentHasher

class ConsistentHasherSpec extends AnyFlatSpec {

  behavior of "the ConsistentHasher data structure"

  behavior of "the constructor"

  val seed = 1
  val hasher = ConsistentHasher(2, seed)

  it should "initialize correctly" in {

    val expected = Map(
      (0 -> Map()),
      (1 -> Map()),
    )
    assert(hasher.showBuckets === expected)
  }

  behavior of "the 'add' method"

  it should "add the given key/value to the right bucket" in {

    val withFoo = hasher.add("foo", "bar")
    val expected = Map(
      (0 -> Map()),
      (1 -> Map(("foo" -> "bar"))),
    )
    assert(withFoo.showBuckets === expected)

    val withBar = withFoo.add("bar", "baz")
    val expected2 = Map(
      (0 -> Map()),
      (1 -> Map(("bar" -> "baz"), ("foo" -> "bar"))),
    )
    assert(withBar.showBuckets === expected2)
  }

}
