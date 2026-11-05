package gameoflife

case class GameResult(
    finalState: State,
    results: Map[Player, (Int, Int)],
    winner: Option[Player]
)
