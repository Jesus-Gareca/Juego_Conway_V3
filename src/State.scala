package gameoflife

case class State(
    boards: Map[Player, GameOfLifeMatrix],
    remainingCells: Map[Player, Int],
    turn: Player
)
