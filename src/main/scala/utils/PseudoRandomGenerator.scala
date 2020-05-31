package utils

// Knuth’s 64-bit linear congruential generator
final case class Seed(long: Long) {
  def next = Seed(long * 6364136223846793005L + 1442695040888963407L)
}
