package gameoflife

//Pruebas parte opcional MatrixList
object MatrixListTests extends App:
  println("Pruebas de MatrixList")
  lazy val base = Matrix(2, 3, List(1, 2, 3, 4, 5, 6))
  lazy val m    = MatrixList(2, 3, List(1, 2, 3, 4, 5, 6))
  lazy val big  = Matrix(4, 5, 0)
  lazy val bigL = MatrixList(4, 5, 0)

  val matrix = List(
    test("get") { assert(m.get(pos(1, 2)) == 6 && m.get(pos(0, 1)) == 2) },
    test("get y update fuera de la matriz lanzan IndexOutOfBoundsException") {
      assert(throws[IndexOutOfBoundsException](m.get(pos(0, 3))))
      assert(throws[IndexOutOfBoundsException](m.update(pos(2, 0), 0)))
    },
    test("update cambia solo una posición y no toca el original") {
      assert(m.update(pos(1, 1), 99).data == List(1, 2, 3, 4, 99, 6))
      assert(m.data == List(1, 2, 3, 4, 5, 6))
    },
    test("neighbors coincide con la versión recursiva en todas las posiciones") {
      assert(allPositions(4, 5).forall(p => bigL.neighbors(p).toSet == big.neighbors(p).toSet))
      assert(allPositions(4, 5).forall(p => bigL.neighbors(p).length == big.neighbors(p).length))
    },
    test("partition y filterNeighbors coinciden con la versión recursiva") {
      assert(m.partition(_ % 2 == 0) == base.partition(_ % 2 == 0))
      assert(allPositions(2, 3).forall(p => m.filterNeighbors(p, _ % 2 == 0).toSet == base.filterNeighbors(p, _ % 2 == 0).toSet))
    },
    test("constructor con posiciones marcadas") {
      assert(MatrixList(2, 3, 0, List(pos(0, 2), pos(1, 0)), 7).data == List(0, 0, 7, 7, 0, 0))
    },
    test("cells (zip) empareja cada posición con su valor") {
      val cells = m.cells
      assert(cells.length == 6)
      assert(cells.head == (pos(0, 0), 1))
      assert(cells.last == (pos(1, 2), 6))
      assert(cells.forall((p, v) => m.get(p) == v))
      assert(cells.map(_._1) == allPositions(2, 3), "las posiciones van en orden de filas")
    }
  )
  summary("MatrixList", matrix)

  println("\nPruebas de GameOfLifeMatrixList")
  def listBoard(b: GameOfLifeMatrix): GameOfLifeMatrixList = GameOfLifeMatrixList(b.rows, b.columns, b.data)
  val glider  = boardWith(8, 8, List(pos(0, 1), pos(1, 2), pos(2, 0), pos(2, 1), pos(2, 2)))
  val blinker = boardWith(5, 5, List(pos(2, 1), pos(2, 2), pos(2, 3)))
  val game = List(
    test("empty y update (devuelve GameOfLifeMatrixList)") {
      val u: GameOfLifeMatrixList = GameOfLifeMatrixList.empty(3, 3).update(pos(1, 1), Alive)
      assert(u.data == boardWith(3, 3, List(pos(1, 1))).data)
    },
    test("countAliveNeighbors, aliveCells y aliveCount coinciden con GameOfLifeMatrix") {
      val l = listBoard(glider)
      assert(allPositions(8, 8).forall(p => l.countAliveNeighbors(p) == glider.countAliveNeighbors(p)))
      assert(l.aliveCells == glider.aliveCells)
      assert(l.aliveCount == 5)
    },
    test("withAlive coincide con GameOfLifeMatrix") {
      assert(GameOfLifeMatrixList.withAlive(4, 4, List(pos(1, 1), pos(3, 0))).data == boardWith(4, 4, List(pos(1, 1), pos(3, 0))).data)
    },
    test("nextGeneration coincide con GameOfLifeMatrix (intermitente y 10 generaciones de planeador)") {
      assert(listBoard(blinker).nextGeneration.data == blinker.nextGeneration.data)
      val viaList   = (1 to 10).foldLeft(listBoard(glider))((b, _) => b.nextGeneration)
      val viaMatrix = (1 to 10).foldLeft(glider)((b, _) => b.nextGeneration)
      assert(viaList.data == viaMatrix.data, drawBoard(viaList.toMatrix))
    }
  )
  summary("GameOfLifeMatrixList", game)
