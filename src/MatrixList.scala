package gameoflife

class MatrixList[T](val rows: Int, val columns: Int, val data: List[T]):
  require(rows >= 0 && columns >= 0 && data.length == rows * columns,
    s"Una matriz $rows x $columns necesita ${rows * columns} datos (hay ${data.length})")

  private def index(p: Position): Int = p.row * columns + p.column
  
  private def positionOf(i: Int): Position = Position(i / columns, i % columns)

    def inside(p: Position): Boolean = ??? //TODO

    def get(p: Position): T = ??? //TODO

    def update(p: Position, value: T): MatrixList[T] = ??? //TODO

    def neighbors(p: Position): List[Position] = ??? //TODO

    def partition(f: T => Boolean): (List[Position], List[Position]) = ??? //TODO

    def filterNeighbors(p: Position, f: T => Boolean): List[Position] = ??? //TODO

    def cells: List[(Position, T)] = ??? //TODO

object MatrixList:
  def apply[T](rows: Int, columns: Int, data: List[T]): MatrixList[T] =
    new MatrixList(rows, columns, data)
  def apply[T](rows: Int, columns: Int, defaultValue: T): MatrixList[T] =
    new MatrixList(rows, columns, List.fill(rows * columns)(defaultValue))
  def apply[T](rows: Int, columns: Int, defaultValue: T, marked: List[Position], markedValue: T): MatrixList[T] =
    new MatrixList(rows, columns,
      List.tabulate(rows * columns)(i =>
        if marked.contains(Position(i / columns, i % columns)) then markedValue else defaultValue))
