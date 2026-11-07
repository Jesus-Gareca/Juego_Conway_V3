package gameoflife
import gameoflife.Player.{Player1, Player2}

import scala.math.Ordering.Implicits.infixOrderingOps
import scala.annotation.tailrec

class Game(val rows: Int, val columns: Int, val budget: Int, val maxGenerations: Int):

    def initialState: State = State(
        boards = Map(
            Player.Player1 -> GameOfLifeMatrix.empty(rows, columns),
            Player.Player2 -> GameOfLifeMatrix.empty(rows, columns)
        ),
        remainingCells = Map(
            Player.Player1 -> budget,
            Player.Player2 -> budget
        ),
        turn = Player.Player1
    )

    def hasFinished(state: State, player: Player): Boolean = {
        state.remainingCells(player) <= 0
    } //TODO

    def placeCell(state: State, p: Position): Either[String, State] = {
        val jugador = state.turn
        val elOtro = otherPlayer(jugador)
        val tablero = state.boards(jugador)
        if (hasFinished(state, jugador)) {
            Left(s"${playerName(jugador)} no tiene más células por colocar")
        } else if (!tablero.inside(p)) {
            Left(s"La posición (${p.row}, ${p.column}) no está dentro del tablero")
        } else if (tablero.get(p) == Alive) {
            Left(s"La posición (${p.row}, ${p.column}) ya tiene una célula viva")
        } else {
            val nuevo_tablero = tablero.update(p, Alive)
            val newboards = state.boards.updated(jugador, nuevo_tablero)
            val newRemainingCells = state.remainingCells + (jugador -> (state.remainingCells(jugador) - 1))
            val nuevo_turno = if (hasFinished(state, elOtro)) {
                jugador
            } else {
                elOtro
            }
            Right(State(newboards, newRemainingCells, nuevo_turno))
        }
    } //TODO

    def placementLoop(state: State, strategies: Map[Player, Strategy]): State = {
        @tailrec
        def aux(state: State, error: Option[String]): State = {
            if (hasFinished(state, Player1) && hasFinished(state, Player2)) {
                state
            } else if (hasFinished(state, state.turn)) {
                aux(state.copy(turn = otherPlayer(state.turn)), None)
            } else {
                val pos = strategies(state.turn).choosePosition(state, state.turn, error)
                placeCell(state, pos) match {
                    case Right(nuevo) => aux(nuevo, None)
                    case Left(mensaje) => aux(state, Some(mensaje))
                }
            }
        }
        aux(state, None)
    } //TODO

    def simulate(board: GameOfLifeMatrix): (Int, Int) = ??? //TODO

    def determineWinner(resultPlayer1: (Int, Int), resultPlayer2: (Int, Int)): Option[Player] = ??? //TODO

    def play(strategies: Map[Player, Strategy]): GameResult = ??? //TODO
