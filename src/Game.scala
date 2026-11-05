package gameoflife
import scala.math.Ordering.Implicits.infixOrderingOps

class Game(val rows: Int, val columns: Int, val budget: Int, val maxGenerations: Int):

    def initialState: State = ??? //TODO

    def hasFinished(state: State, player: Player): Boolean = ??? //TODO

    def placeCell(state: State, p: Position): Either[String, State] = ??? //TODO

    def placementLoop(state: State, strategies: Map[Player, Strategy]): State = ??? //TODO

    def simulate(board: GameOfLifeMatrix): (Int, Int) = ??? //TODO

    def determineWinner(resultPlayer1: (Int, Int), resultPlayer2: (Int, Int)): Option[Player] = ??? //TODO

    def play(strategies: Map[Player, Strategy]): GameResult = ??? //TODO
