package tennis

class TennisGame1(val player1Name: String, val player2Name: String) extends TennisGame {
  var player1 = Player("player1")
  var player2 = Player("player2")

  def players = Seq(player1, player2)

  def wonPoint(playerName: String) {
    players.find(_.name == playerName).foreach(_.increaseScore())
  }

  def calculateScore() = (player1, player2) match {
    case WIN(player) =>
      s"Win for ${player.name}"

    case TIGHT(score) if isAdvantage(score) =>
      "Deuce"

    case TIGHT(score) =>
      s"${score.label}-All"

    case ADVANTAGE(player) =>
      s"Advantage ${player.name}"

    case CLASSIC(score1, score2) =>
      s"${score1.label}-${score2.label}"
  }

  def isAdvantage(score: TennisScore): Boolean = {
    score.value >= 3
  }
}