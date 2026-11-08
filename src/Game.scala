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

    // newBoard para definir la posicion de la sigueinte generacion
    // Guardamos las posiciones anteriores con Set y comprobamos si currentBoard se repite
    def simulate(board: GameOfLifeMatrix): (Int, Int) =
        @tailrec
        def loop(currentBoard: GameOfLifeMatrix, generation: Int, previousBoards: Set[GameOfLiveMatrix]): (Int, Int) =
            val newBoard = currentBoard.nextGeneration
            if (i + 1 >= maxGenerations || previousBoards.contains(newBoard))
                (i + 1, newBoard.aliveCount)
            else
                loop(newBoard, i + 1, previousBoards + newBoard)
        loop(board, 0, Set(board))        

    //Consideramos primero si los resultados son iguales, si son distintos quien tiene más generaciones
    //Por último consideramos el número de células vivas como se menciona en las indicaciones
    def determineWinner(resultPlayer1: (Int, Int), resultPlayer2: (Int, Int)): Option[Player] =
        if (resultPlayer1 == resultPlayer2) None
        else if (resultPlayer1._1 != resultPlayer2._1)
            if (resultPlayer1._1 > resultPlayer2._1) Some(Player.Player1) else Some(Player.Player2)
        else
            if (resultPlayer1._2 > resultPlayer2._2) Some(Player.Player1) else Some(Player.Player2)

    def play(strategies: Map[Player, Strategy]): GameResult =
        val finalState = placementLoop(initialState, strategies)

        val resultPlayer1 = simulate(finalState.boards(Player.Player1))
        val resultPlayer2 = simulate(finalState.boards(Player.Player2))

        val results = Map(
            Player.Player1 -> resultPlayer1,
            Player.Player2 -> resultPlayer2
        )
        val winner = determineWinner(resultPlayer1, resultPlayer2)
        GameResult(finalState, results, winner)