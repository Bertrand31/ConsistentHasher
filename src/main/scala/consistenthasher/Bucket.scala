package consistenthasher

final case class Bucket(id: Int, angle: Int, data: Map[String, String] = Map.empty) {

  def insert(key: String, value: String): Bucket =
    copy(data=this.data + (key -> value))

  def remove(key: String): Bucket =
    copy(data=this.data - key)
}
