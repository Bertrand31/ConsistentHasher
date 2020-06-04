package utils

// Knuth’s 64-bit linear congruential generator
final case class Seed(private val long: Long) {

  def gen: (Long, Seed) = {
    val nextSeed = Seed(long * 6364136223846793005L + 1442695040888963407L)
    (this.long, nextSeed)
  }
}
