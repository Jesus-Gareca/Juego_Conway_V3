package gameoflife

case class GameOfLifeMatrix(override val rows: Int, override val columns: Int, override val data: List[Cell])
    extends Matrix[Cell](rows, columns, data):

    override def update(p: Position, value: Cell): GameOfLifeMatrix = ??? //TODO

    def aliveCells: List[Position] = ??? //TODO

    def aliveCount: Int = ??? //TODO

    def countAliveNeighbors(p: Position): Int = ??? //TODO

    def nextGeneration: GameOfLifeMatrix = ??? //TODO

    //¡No tocar!
    def show(title: String = ""): String =
      val reset  = "\u001b[0m"
      val green  = "\u001b[92m"
      val dark   = "\u001b[38;5;237m"
      val dim    = "\u001b[90m"
      val frame  = "\u001b[38;5;67m"
      val yellow = "\u001b[93m"
      def square(c: Cell): String = c match
        case Alive => s"$green██$reset"
        case Dead  => s"$dark██$reset"
      val titleLine = if title.isEmpty then Nil else List(s"  $yellow$title$reset")
      val header    = s"$dim      " + List.range(0, columns).map(c => f"$c%2d").mkString(" ") + reset
      val top       = s"$frame    ┌" + "─" * (3 * columns + 1) + s"┐$reset"
      val bottom    = s"$frame    └" + "─" * (3 * columns + 1) + s"┘$reset"
      val lines     = data.grouped(columns).toList.zipWithIndex.map: (row, r) =>
        s"$dim ${f"$r%2d"} $reset$frame│$reset " + row.map(c => square(c) + " ").mkString + s"$frame│$reset"
      (titleLine ++ List(header, top) ++ lines ++ List(bottom)).mkString("\n")

    override def toString: String = show()

object GameOfLifeMatrix:
    def empty(rows: Int, columns: Int): GameOfLifeMatrix = ??? //TODO

    def withAlive(rows: Int, columns: Int, alive: List[Position]): GameOfLifeMatrix = ??? //TODO
