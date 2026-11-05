package gameoflife

import scala.annotation.tailrec

class Matrix[T](val rows: Int, val columns: Int, val data: List[T]):
  require(rows >= 0 && columns >= 0 && data.length == rows * columns,
    s"Una matriz $rows x $columns necesita ${rows * columns} datos (hay ${data.length})")

  private def index(p: Position): Int = p.row * columns + p.column

  private def positionOf(i: Int): Position = Position(i / columns, i % columns)

  def inside(p: Position): Boolean = p.row>=0 && p.row<rows && p.column>=0 && p.column<columns //TODO

  def get(p: Position): T = //TODO
    if (inside(p))
      @annotation.tailrec
      def aux(lista: List[T], indice: Int): T = lista match
       case head :: tail => if (indice == 0) head else aux(tail, indice -1)
       case Nil => throw new IndexOutOfBoundsException()
      aux(data, index(p));
    else throw new IndexOutOfBoundsException()

  def update(p: Position, value: T): Matrix[T] = //TODO
    if(inside(p))
      @annotation.tailrec
      def aux(lista: List[T], indice: Int, acc: List[T]): List[T] = lista match
        case head :: tail => //Recorremos la lista con "indice - 1", cuando indice = 0 hemos encontrado el elemento
          val elemento = if (indice == 0) value else head //Añadimos value(nuevo) si hemos encontrado el numero, si no, lo añadimos al acumulador
          aux(tail, indice - 1, elemento :: acc)
        case Nil => acc.reverse
      Matrix(rows, columns, aux(data, index(p), Nil))
    else throw new IndexOutOfBoundsException()

  def neighbors(p: Position): List[Position] =
    val candidatas = List(
      Position(p.row - 1, p.column - 1),
      Position(p.row - 1, p.column),
      Position(p.row - 1, p.column + 1),
      Position(p.row, p.column - 1),
      Position(p.row, p.column + 1),
      Position(p.row + 1, p.column - 1),
      Position(p.row + 1, p.column),
      Position(p.row + 1, p.column + 1)
    )

    @annotation.tailrec
    def aux(lista: List[Position], acc: List[Position]): List[Position] = lista match
        case head :: tail => if (inside(head)) aux(tail, head :: acc) else aux(tail, acc)
        case Nil => acc.reverse
    aux(candidatas, Nil)

  def partition(f: T => Boolean): (List[Position], List[Position]) = //Separamos todos los elementos de la matriz en dos listas dependiendo si cumplen f o no
    @annotation.tailrec //Aunque recorramos toda la lista usamos "indice" para movernos para asi obtener las posiciones (X,Y) de cada elemento
    def aux(lista: List[T], indice: Int, cumplen: List[Position], noCumplen: List[Position]): (List[Position], List[Position]) = lista match
      case head :: tail => val posicion = positionOf(indice) //Obetemos las (X,Y) del primer elemento en cada iteracion
        if (f(head)) aux(tail, indice + 1, posicion :: cumplen, noCumplen) //Si (X,Y) cumple f, se añade al principio del acumulador Cumplen
        else aux(tail, indice + 1, cumplen, posicion :: noCumplen) //Si (X,Y) no cumple f, se añade al principio del acumulador de NoCumplen
      case Nil => (cumplen.reverse, noCumplen.reverse)
    aux(data, 0, Nil, Nil)  //Iniciamos aux con los datos de la matriz, indice = 0, y los acumuladores vacios

  def filterNeighbors(p: Position, f: T => Boolean): List[Position] =
    @annotation.tailrec
    def aux(lista: List[Position], acc: List[Position]): List[Position] = lista match
      case head :: tail => if (f(get(head))) aux(tail, head :: acc) else aux(tail, acc)
      case Nil => acc.reverse
    aux(neighbors(p), Nil)

  def apply[T](rows: Int, columns: Int, defaultValue: T): Matrix[T] =
    @annotation.tailrec
    def aux(n: Int, acc: List[T]): List[T] = if (n==0) acc else aux(n-1, defaultValue :: acc)
    new Matrix(rows, columns, aux(rows * columns, Nil))

  def apply[T](rows: Int, columns: Int, defaultValue: T, marked: List[Position], markedValue: T): Matrix[T] =
    @annotation.tailrec
    def aux(lista: List[Position], matriz: Matrix[T]): Matrix[T] = lista match
      case head :: tail => aux(tail, matriz.update(head, markedValue))
      case Nil => matriz
    aux(marked, apply(rows, columns, defaultValue))
