package gameoflife

import scala.annotation.tailrec

class Matrix[T](val rows: Int, val columns: Int, val data: List[T]):
  require(rows >= 0 && columns >= 0 && data.length == rows * columns,
    s"Una matriz $rows x $columns necesita ${rows * columns} datos (hay ${data.length})")

  private def index(p: Position): Int = p.row * columns + p.column

  private def positionOf(i: Int): Position = Position(i / columns, i % columns)

  // Comprueba que la posición este dentro del tablero
  def inside(p: Position): Boolean = p.row>=0 && p.row<rows && p.column>=0 && p.column<columns //TODO

  // Busca el valor en la posición p recorriendo la lista hasta llegar al índice
  def get(p: Position): T = //TODO
    if (inside(p))
      @annotation.tailrec
      def aux(lista: List[T], indice: Int): T = lista match
       case head :: tail => if (indice == 0) head else aux(tail, indice -1) // Avanza hasta que el índice sea 0
       case Nil => throw new IndexOutOfBoundsException()
      aux(data, index(p));
    else throw new IndexOutOfBoundsException()

  // Devuelve una copia de la matriz con el valor cambiado en la posición p
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

  // Devuelve las 8 posiciones vecinas que están dentro del tablero
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
      // Si está dentro, la guardamos; si no, la ignoramos
      case head :: tail => if (inside(head)) aux(tail, head :: acc) else aux(tail, acc)
      case Nil => acc.reverse
    aux(candidatas, Nil)

  // Separa las posiciones en dos listas: las que cumplen "f" y las que no
  def partition(f: T => Boolean): (List[Position], List[Position]) = //Separamos todos los elementos de la matriz en dos listas dependiendo si cumplen f o no
    @annotation.tailrec //Aunque recorramos toda la lista usamos "indice" para movernos para asi obtener las posiciones (X, Y) de cada elemento
    def aux(lista: List[T], indice: Int, cumplen: List[Position], noCumplen: List[Position]): (List[Position], List[Position]) = lista match
      case head :: tail => val posicion = positionOf(indice) //Obtenemos las (X, Y) del primer elemento en cada iteración
        if (f(head)) aux(tail, indice + 1, posicion :: cumplen, noCumplen) //Si (X, Y) cumple f, se añade al principio del acumulador Cumplen
        else aux(tail, indice + 1, cumplen, posicion :: noCumplen) //Si (X, Y) no cumple f, se añade al principio del acumulador de NoCumplen
      case Nil => (cumplen.reverse, noCumplen.reverse)
    aux(data, 0, Nil, Nil)  //Iniciamos aux con los datos de la matriz, indice = 0, y los acumuladores vacíos

  // Filtra las vecinas de p cuyo valor cumpla la función f
  def filterNeighbors(p: Position, f: T => Boolean): List[Position] =
    @annotation.tailrec
    def aux(lista: List[Position], acc: List[Position]): List[Position] = lista match
      case head :: tail => if (f(get(head))) aux(tail, head :: acc) else aux(tail, acc) // Comprueba el valor con get
      case Nil => acc.reverse
    aux(neighbors(p), Nil)

object Matrix:
  def apply[T](rows: Int, columns: Int, data: List[T]): Matrix[T] =
    new Matrix(rows, columns, data)

  // Crea una matriz llena del valor por defecto
  def apply[T](rows: Int, columns: Int, defaultValue: T): Matrix[T] =
    @annotation.tailrec
    def aux(n: Int, acc: List[T]): List[T] = if (n==0) acc else aux(n-1, defaultValue :: acc) // Añade "n" veces el valor por defecto
    new Matrix(rows, columns, aux(rows * columns, Nil))

  // Crea una matriz con valor por defecto, pero cambiando las posiciones de "marked"
  def apply[T](rows: Int, columns: Int, defaultValue: T, marked: List[Position], markedValue: T): Matrix[T] = {
    @tailrec
    def aux(i: Int, acc: List[T]): List[T] = {
      if (i == rows * columns) {
        acc.reverse
      } else {
        val pos = Position(i / columns, i % columns) // Posición actual del índice "i"
        val comprobar = if (marked.contains(pos)) {markedValue} else {defaultValue}
        aux(i + 1, comprobar :: acc) // Avanzamos al siguiente índice
      }
    }
    new Matrix(rows, columns, aux(0, Nil))
  } //TODO
