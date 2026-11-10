package gameoflife

class MatrixList[T](val rows: Int, val columns: Int, val data: List[T]):
  require(rows >= 0 && columns >= 0 && data.length == rows * columns,
    s"Una matriz $rows x $columns necesita ${rows * columns} datos (hay ${data.length})")

  private def index(p: Position): Int = p.row * columns + p.column
  
  private def positionOf(i: Int): Position = Position(i / columns, i % columns)

    def inside(p: Position): Boolean = {
      p.row>=0 && p.row<rows && p.column>=0 && p.column<columns
    }
    //TODO

    def get(p: Position): T = {
      if (inside(p)) {
        data(index(p))
      } else {
        throw new IndexOutOfBoundsException()
      }
    }//TODO

    def update(p: Position, value: T): MatrixList[T] = {
      if (inside(p)) {
        new MatrixList(rows, columns, data.updated(index(p), value))
      } else {
        throw new IndexOutOfBoundsException()
      }
    } //TODO

    def neighbors(p: Position): List[Position] = {
      val candidatos = List(Position(p.row - 1, p.column - 1),
        Position(p.row - 1, p.column),
        Position(p.row - 1, p.column + 1),
        Position(p.row, p.column - 1),
        Position(p.row, p.column + 1),
        Position(p.row + 1, p.column - 1),
        Position(p.row + 1, p.column),
        Position(p.row + 1, p.column + 1))
      candidatos.filter(inside)
    } //TODO

    def partition(f: T => Boolean): (List[Position], List[Position]) = {
      val (validas, noValidas) = cells.partition { case (_, valor) => f(valor) }
      (validas.map { case (posicion, _) => posicion }, noValidas.map { case (posicion, _) => posicion})
    } //TODO

    def filterNeighbors(p: Position, f: T => Boolean): List[Position] = {
      neighbors(p).filter(np => f(get(np)))
    } //TODO

  // Empareja cada valor con su índice, y usa map para convertir el índice en Position
    def cells: List[(Position, T)] = {
      data.zipWithIndex.map {
        case (valor, indice) => (positionOf(indice), valor)}
    } //TODO

object MatrixList:
  def apply[T](rows: Int, columns: Int, data: List[T]): MatrixList[T] =
    new MatrixList(rows, columns, data)
  def apply[T](rows: Int, columns: Int, defaultValue: T): MatrixList[T] =
    new MatrixList(rows, columns, List.fill(rows * columns)(defaultValue))
  def apply[T](rows: Int, columns: Int, defaultValue: T, marked: List[Position], markedValue: T): MatrixList[T] =
    new MatrixList(rows, columns,
      List.tabulate(rows * columns)(i =>
        if marked.contains(Position(i / columns, i % columns)) then markedValue else defaultValue))
