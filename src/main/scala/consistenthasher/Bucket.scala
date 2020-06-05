package consistenthasher

final case class Bucket[A, B](id: Int, angle: Float, data: Map[A, B]) {

  def insert(key: A, value: B): Bucket[A, B] =
    copy(data=this.data + (key -> value))

  def remove(key: A): Bucket[A, B] =
    copy(data=this.data - key)
}
