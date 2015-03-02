package tennis

case class Player(name: String) {
  var score: Int = 0

  def increaseScore(): Unit = {
    score += 1
  }

  override def toString = s"$name: $score"
}

object Player {
  implicit val playerOrdering: Ordering[Player] = Ordering.by((player: Player) => player.score)
}