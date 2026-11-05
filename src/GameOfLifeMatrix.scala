package gameoflife

import gameoflife.GameOfLifeMatrix.withAlive

import scala.annotation.tailrec

case class GameOfLifeMatrix(override val rows: Int, override val columns: Int, override val data: List[Cell])
    extends Matrix[Cell](rows, columns, data):

  // El update heredado ya valida `inside` y lanza IndexOutOfBoundsException si toca
  // Devuelve Matrix[Cell], así que re-envolvemos su .data para acotar el tipo a GameOfLifeMatrix
  // Se usa super.update, no update solo para no entrar en recursión infinita
    override def update(p: Position, value: Cell): GameOfLifeMatrix = {
      val lista = super.update(p, value).data
      GameOfLifeMatrix(rows, columns, lista)
    } //TODO

  // partition(_ == Alive) parte las posiciones en (vivas, muertas) en orden de filas
  // Solo nos interesa la primera componente de la tupla; "_" descarta la segunda
    def aliveCells: List[Position] = {
      val (vivas, _) = partition(_ == Alive)
      vivas
    } //TODO

  // Vemos la longitud de aliveCells porque ya las ha "contado" la longitud de la lista
  // es la cantidad de células vivas
    def aliveCount: Int = {
      aliveCells.length
    } //TODO

  // filterNeighbors ya excluye p y descarta las vecinas fuera del tablero
  // Con _ == Alive nos quedamos solo con las vivas y con length las contamos
    def countAliveNeighbors(p: Position): Int = {
      val vecinos = filterNeighbors(p, _ == Alive)
      vecinos.length
    } //TODO

  // Para aplicar las reglas B3/S23 decomponemos en dos montones:
  // supervivientes = las que ya estaban vivas y se mantienen porque tienen 2 0 3 vecinas vivas
  // reviven = las que toman vida porque tienen 3 vecinas vivas
    def nextGeneration: GameOfLifeMatrix = {
      val (vivas, muertas) = partition(_ == Alive)
      // Recorre las vivas y comprueba si tiene el número de vecinas vivas necesario para
      // mantener, si es el caso se añade en la lista y si no se pasa a la siguiente viva
      @tailrec
      def supervivientes(l: List[Position], acc: List[Position]): List[Position] = {
        l match {
          case Nil => acc.reverse
          case h :: t =>
            val vecinos_vivos = countAliveNeighbors(h)
            if (vecinos_vivos == 2 || vecinos_vivos == 3) {
              supervivientes(t, h :: acc)
            } else {
              supervivientes(t, acc)
            }
        }
      }
      // Lo mismo que el otro pero con las muertas y comprobando si tiene 3 vecinas vivas
      @tailrec
      def reviven(l: List[Position], acc: List[Position]): List[Position] = {
        l match {
          case Nil => acc.reverse
          case h :: t =>
            if (countAliveNeighbors(h) == 3) {
              reviven(t, h :: acc)
            } else {
              reviven(t, acc)
            }
        }
      }
      // Suma todas las vivas de supervivientes y reviven y crea el tablero, no se duplican
      // porque un montón viven de vuvas y otro de muertas, es decir, no se evalua
      // dos veces la misma célula
      val vivas_totales = supervivientes(vivas, Nil) ++ reviven(muertas, Nil)
      GameOfLifeMatrix.withAlive(rows, columns, vivas_totales)
    } //TODO

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
  // Con GameOfLifeMatrix construimos un tablero con la lista que consiguimos al pasar
  // la matriz con todo células muertas que creamos con Matrix y pasarle por data, es decir
  // Matrix crea una Matrix[Cell] y data lo convierte en una List[Cell] que es lo que pide
  // GameOfLifeMatrix
    def empty(rows: Int, columns: Int): GameOfLifeMatrix = {
      GameOfLifeMatrix(rows, columns, Matrix(rows, columns, Dead).data)
    } //TODO

  // Lo mismo pero con la matriz combinando vivos y muertos
    def withAlive(rows: Int, columns: Int, alive: List[Position]): GameOfLifeMatrix = {
      GameOfLifeMatrix(rows, columns, Matrix(rows, columns, Dead, alive, Alive).data)
    } //TODO
