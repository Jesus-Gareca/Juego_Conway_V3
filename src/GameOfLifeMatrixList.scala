package gameoflife

case class GameOfLifeMatrixList(override val rows: Int, override val columns: Int, override val data: List[Cell])
    extends MatrixList[Cell](rows, columns, data):

    override def update(p: Position, value: Cell): GameOfLifeMatrixList = {
        val lista = super.update(p, value).data
        GameOfLifeMatrixList(rows, columns, lista)
    } //TODO

    def aliveCells: List[Position] = {
        val (vivas, _) = partition(_ == Alive)
        vivas
    } //TODO

    def aliveCount: Int = {
        aliveCells.length
    } //TODO

    def countAliveNeighbors(p: Position): Int = {
        val vecinos = filterNeighbors(p, _ == Alive)
        vecinos.length
    } //TODO

    def nextGeneration: GameOfLifeMatrixList = {
        val (vivas, muertas) = partition(_ == Alive)
        val supervivientes = vivas.filter(p => countAliveNeighbors(p) == 2 || countAliveNeighbors(p) == 3)
        val reviven = muertas.filter(p => countAliveNeighbors(p) == 3)
        GameOfLifeMatrixList.withAlive(rows, columns, supervivientes ++ reviven)
    } //TODO
    def toMatrix: GameOfLifeMatrix = GameOfLifeMatrix(rows, columns, data)
    override def toString: String = toMatrix.show()

object GameOfLifeMatrixList:
    def empty(rows: Int, columns: Int): GameOfLifeMatrixList = {
        GameOfLifeMatrixList(rows, columns, MatrixList(rows, columns, Dead).data)
    } //TODO

    def withAlive(rows: Int, columns: Int, alive: List[Position]): GameOfLifeMatrixList = {
        GameOfLifeMatrixList(rows, columns, MatrixList(rows, columns, Dead, alive, Alive).data)
    } //TODO
