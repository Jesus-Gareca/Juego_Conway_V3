package gameoflife

//Pruebas de GameOfLifeMatrix: update, vecinas vivas, reglas del juego y dibujo.
object GameOfLifeMatrixTests extends App:
  println("Pruebas de GameOfLifeMatrix")
  val blinkerH      = boardWith(5, 5, List(pos(2, 1), pos(2, 2), pos(2, 3)))
  val blinkerV      = boardWith(5, 5, List(pos(1, 2), pos(2, 2), pos(3, 2)))
  val block         = boardWith(5, 5, List(pos(1, 1), pos(1, 2), pos(2, 1), pos(2, 2)))
  val glider        = boardWith(8, 8, List(pos(0, 1), pos(1, 2), pos(2, 0), pos(2, 1), pos(2, 2)))
  val gliderShifted = boardWith(8, 8, List(pos(1, 2), pos(2, 3), pos(3, 1), pos(3, 2), pos(3, 3)))

  val basics = List(
    test("GameOfLifeMatrix.empty crea un tablero con todas las células muertas") {
      val e = GameOfLifeMatrix.empty(3, 4)
      assert(e.rows == 3 && e.columns == 4)
      assert(e.data == List.fill(12)(Dead))
    },
    test("GameOfLifeMatrix.withAlive crea un tablero con vivas exactamente esas posiciones") {
      assert(GameOfLifeMatrix.withAlive(5, 5, List(pos(1, 1), pos(1, 2), pos(2, 1), pos(2, 2))) == block)
      assert(GameOfLifeMatrix.withAlive(3, 4, Nil) == emptyBoard(3, 4))
    },
    test("es una Matrix[Cell]: hereda get, neighbors, partition...") {
      val m: Matrix[Cell] = block
      assert(m.get(pos(1, 1)) == Alive && m.get(pos(0, 0)) == Dead)
    },
    test("update devuelve un GameOfLifeMatrix y no modifica el original") {
      val e = emptyBoard(3, 3)
      val u: GameOfLifeMatrix = e.update(pos(1, 2), Alive)
      assert(u == boardWith(3, 3, List(pos(1, 2))), drawBoard(u))
      assert(e == emptyBoard(3, 3))
    },
    test("dos tableros con el mismo contenido son iguales (case class)") {
      assert(boardWith(3, 3, List(pos(0, 0))) == boardWith(3, 3, List(pos(0, 0))))
      assert(boardWith(3, 3, List(pos(0, 0))) != boardWith(3, 3, List(pos(0, 1))))
      assert(Set(block, boardWith(5, 5, List(pos(1, 1), pos(1, 2), pos(2, 1), pos(2, 2)))).size == 1)
    },
    test("aliveCells devuelve las posiciones vivas en orden de filas") {
      assert(glider.aliveCells == List(pos(0, 1), pos(1, 2), pos(2, 0), pos(2, 1), pos(2, 2)), s"aliveCells = ${glider.aliveCells}")
      assert(emptyBoard(3, 3).aliveCells.isEmpty)
    },
    test("aliveCount cuenta las células vivas") {
      assert(emptyBoard(4, 4).aliveCount == 0)
      assert(block.aliveCount == 4 && glider.aliveCount == 5)
    },
    test("countAliveNeighbors") {
      val full = boardWith(3, 3, (for r <- 0 to 2; c <- 0 to 2 yield pos(r, c)).toList)
      assert(full.countAliveNeighbors(pos(1, 1)) == 8, "centro de un 3x3 lleno")
      assert(full.countAliveNeighbors(pos(0, 0)) == 3, "esquina de un 3x3 lleno")
      assert(blinkerH.countAliveNeighbors(pos(2, 2)) == 2)
      assert(blinkerH.countAliveNeighbors(pos(1, 2)) == 3, "cuenta vecinas vivas de una celda muerta")
      assert(blinkerH.countAliveNeighbors(pos(0, 0)) == 0)
    }
  )
  summary("GameOfLifeMatrix (básico)", basics)

  println("\nPruebas de las reglas (nextGeneration)")
  val rules = List(
    test("un tablero vacío sigue vacío") {
      assert(emptyBoard(4, 4).nextGeneration == emptyBoard(4, 4))
    },
    test("una célula sola muere (infrapoblación)") {
      assert(boardWith(3, 3, List(pos(1, 1))).nextGeneration == emptyBoard(3, 3))
    },
    test("el bloque es una vida estática") {
      assert(block.nextGeneration == block)
    },
    test("el intermitente (blinker) alterna horizontal <-> vertical") {
      assert(blinkerH.nextGeneration == blinkerV, drawBoard(blinkerH.nextGeneration))
      assert(blinkerV.nextGeneration == blinkerH)
    },
    test("nace una célula muerta con exactamente 3 vecinas (L de 3 células -> bloque)") {
      val l = boardWith(4, 4, List(pos(1, 1), pos(1, 2), pos(2, 1)))
      assert(l.nextGeneration == boardWith(4, 4, List(pos(1, 1), pos(1, 2), pos(2, 1), pos(2, 2))))
    },
    test("sobrepoblación: una célula con 4 vecinas muere") {
      val cross = boardWith(5, 5, List(pos(2, 2), pos(1, 2), pos(3, 2), pos(2, 1), pos(2, 3)))
      assert(cross.nextGeneration.get(pos(2, 2)) == Dead)
    },
    test("los bordes del tablero son muertos (no hay tablero toroidal)") {
      val edge = boardWith(3, 3, List(pos(0, 0), pos(0, 1), pos(0, 2)))
      assert(edge.nextGeneration == boardWith(3, 3, List(pos(0, 1), pos(1, 1))))
    },
    test("el planeador (glider) avanza una casilla en diagonal cada 4 generaciones") {
      val after4 = (1 to 4).foldLeft(glider)((b, _) => b.nextGeneration)
      assert(after4 == gliderShifted, drawBoard(after4))
    },
    test("nextGeneration no modifica el tablero original") {
      blinkerH.nextGeneration
      assert(blinkerH == boardWith(5, 5, List(pos(2, 1), pos(2, 2), pos(2, 3))))
    }
  )
  summary("Reglas (nextGeneration)", rules)
