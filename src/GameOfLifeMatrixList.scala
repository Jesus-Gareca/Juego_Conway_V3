package gameoflife

case class GameOfLifeMatrixList(override val rows: Int, override val columns: Int, override val data: List[Cell])
    extends MatrixList[Cell](rows, columns, data):

    override def update(p: Position, value: Cell): GameOfLifeMatrixList = ??? //TODO

    def aliveCells: List[Position] = ??? //TODO

    def aliveCount: Int = ??? //TODO

    def countAliveNeighbors(p: Position): Int = ??? //TODO

    def nextGeneration: GameOfLifeMatrixList = ??? //TODO
    def toMatrix: GameOfLifeMatrix = GameOfLifeMatrix(rows, columns, data)
    override def toString: String = toMatrix.show()

object GameOfLifeMatrixList:
    def empty(rows: Int, columns: Int): GameOfLifeMatrixList = ??? //TODO

    def withAlive(rows: Int, columns: Int, alive: List[Position]): GameOfLifeMatrixList = ??? //TODO
