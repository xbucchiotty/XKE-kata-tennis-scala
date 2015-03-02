package tennis

case class TennisScore(label: String, value: Int)

object TennisScore {
  def unapply(player: Player): Option[TennisScore] = player.score match {
    case 0 => Some(TennisScore("Love", 0))
    case 1 => Some(TennisScore("Fifteen", 1))
    case 2 => Some(TennisScore("Thirty", 2))
    case 3 => Some(TennisScore("Forty", 3))
    case x if x > 3 => Some(TennisScore("", x))
    case _ => None
  }
}

object WIN {
  def unapply(playerScores: (Player, Player)): Option[Player] = {
    val (player1, player2) = playerScores

    def players = Seq(player1, player2)

    ADVANTAGE.unapply(playerScores) match {
      case Some(winner) if Math.abs(player1.score - player2.score) == 2 =>
        Some(winner)

      case Some(winner) =>
        None

      case None =>
        players.find(_.score == 4)
    }
  }
}

object TIGHT {
  def unapply(playerScores: (Player, Player)): Option[TennisScore] = {
    val (player1, player2) = playerScores

    if (player1.score == player2.score) {
      TennisScore.unapply(player1)
    } else {
      None
    }
  }
}

object ADVANTAGE {
  def unapply(playerScores: (Player, Player)): Option[Player] = {
    val (player1, player2) = playerScores

    def advantage = player1.score >= 3 && player2.score >= 3
    def players = Seq(player1, player2)

    if (advantage) {
      Some(players.max)
    }
    else {
      None
    }
  }
}

object CLASSIC {
  def unapply(playerScores: (Player, Player)): Option[(TennisScore, TennisScore)] = playerScores match {
    case (TennisScore(score1), TennisScore(score2)) => Some(score1, score2)
    case _ => None
  }
}