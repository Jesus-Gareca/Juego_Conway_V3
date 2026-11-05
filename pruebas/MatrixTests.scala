package gameoflife

//Pruebas de la clase genérica Matrix. Ejecutad MatrixTests cuando tengáis Matrix terminada.
object MatrixTests extends App:
  println("Pruebas de Matrix")
  // Matriz de ejemplo 2 filas x 3 columnas:   1 2 3
  //                                           4 5 6
  lazy val m = Matrix(2, 3, List(1, 2, 3, 4, 5, 6))

  val results = List(
    test("Matrix(rows, columns, data) usa la lista como datos") {
      assert(m.rows == 2 && m.columns == 3)
      assert(m.data == List(1, 2, 3, 4, 5, 6), s"data = ${m.data}")
    },
    test("Matrix(rows, columns, defaultValue, marked, markedValue) marca solo esas posiciones") {
      val z = Matrix(2, 3, 0, List(pos(0, 2), pos(1, 0)), 7)
      assert(z.data == List(0, 0, 7, 7, 0, 0), s"data = ${z.data}")
      assert(Matrix(2, 2, 0, Nil, 7).data == List(0, 0, 0, 0))
      assert(Matrix(1, 1, 0, List(pos(0, 0)), 7).data == List(7))
    },
    test("Matrix(rows, columns, defaultValue) rellena todas las celdas") {
      val z = Matrix(2, 3, 0)
      assert(z.data == List(0, 0, 0, 0, 0, 0), s"data = ${z.data}")
      assert(Matrix(0, 3, 0).data.isEmpty, "una matriz sin filas no tiene datos")
    },
    test("no se puede crear una matriz con un número de datos incorrecto") {
      assert(throws[IllegalArgumentException](Matrix(2, 3, List(1, 2))))
    },
    test("get devuelve el valor de cada posición (orden de filas)") {
      assert(m.get(pos(0, 0)) == 1)
      assert(m.get(pos(0, 2)) == 3)
      assert(m.get(pos(1, 0)) == 4)
      assert(m.get(pos(1, 2)) == 6)
    },
    test("get fuera de la matriz lanza IndexOutOfBoundsException") {
      assert(throws[IndexOutOfBoundsException](m.get(pos(2, 0))))
      assert(throws[IndexOutOfBoundsException](m.get(pos(0, 3))), "(0,3) no es (1,0): hay que comprobar la columna")
      assert(throws[IndexOutOfBoundsException](m.get(pos(-1, 0))))
    },
    test("inside distingue posiciones dentro y fuera") {
      assert(m.inside(pos(0, 0)) && m.inside(pos(1, 2)))
      assert(!m.inside(pos(-1, 0)) && !m.inside(pos(0, -1)))
      assert(!m.inside(pos(2, 0)) && !m.inside(pos(0, 3)))
    },
    test("update cambia solo la posición indicada") {
      val u = m.update(pos(1, 1), 99)
      assert(u.data == List(1, 2, 3, 4, 99, 6), s"data = ${u.data}")
      assert(u.rows == 2 && u.columns == 3)
    },
    test("update no modifica la matriz original (inmutabilidad)") {
      m.update(pos(0, 0), 100)
      assert(m.data == List(1, 2, 3, 4, 5, 6))
    },
    test("update funciona en la primera y en la última posición") {
      assert(m.update(pos(0, 0), 0).data == List(0, 2, 3, 4, 5, 6))
      assert(m.update(pos(1, 2), 0).data == List(1, 2, 3, 4, 5, 0))
    },
    test("update fuera de la matriz lanza IndexOutOfBoundsException") {
      assert(throws[IndexOutOfBoundsException](m.update(pos(0, 3), 0)))
    },
    test("partition reparte las posiciones según su valor, en orden de filas") {
      val (even, odd) = m.partition(_ % 2 == 0)
      assert(even == List(pos(0, 1), pos(1, 0), pos(1, 2)), s"las que cumplen = $even")
      assert(odd == List(pos(0, 0), pos(0, 2), pos(1, 1)), s"las que no = $odd")
    },
    test("partition: si todas cumplen, la segunda lista está vacía (y al revés)") {
      assert(m.partition(_ > 0) == (allPositions(2, 3), Nil))
      assert(m.partition(_ > 100) == (Nil, allPositions(2, 3)))
    },
    test("partition no modifica la matriz") {
      m.partition(_ > 3)
      assert(m.data == List(1, 2, 3, 4, 5, 6))
    },
    test("neighbors: una esquina tiene 3 vecinas") {
      val n = Matrix(5, 5, 0).neighbors(pos(0, 0))
      assert(n.toSet == Set(pos(0, 1), pos(1, 0), pos(1, 1)), s"neighbors = $n")
      assert(n.length == 3, "hay vecinas repetidas")
    },
    test("neighbors: un borde tiene 5 vecinas") {
      assert(Matrix(5, 5, 0).neighbors(pos(0, 2)).length == 5)
      assert(Matrix(5, 5, 0).neighbors(pos(3, 4)).length == 5)
    },
    test("neighbors: una celda interior tiene 8 vecinas y no se incluye a sí misma") {
      val n = Matrix(5, 5, 0).neighbors(pos(2, 2))
      assert(n.length == 8 && n.toSet.size == 8)
      assert(!n.contains(pos(2, 2)))
      assert(n.toSet == (for r <- 1 to 3; c <- 1 to 3 if !(r == 2 && c == 2) yield pos(r, c)).toSet)
    },
    test("neighbors: en una matriz 1x1 no hay vecinas") {
      assert(Matrix(1, 1, 0).neighbors(pos(0, 0)).isEmpty)
    },
    test("filterNeighbors devuelve las vecinas cuyo valor cumple el criterio") {
      // 1 2 3
      // 4 5 6
      assert(m.filterNeighbors(pos(0, 0), _ % 2 == 0).toSet == Set(pos(0, 1), pos(1, 0)), "vecinas de (0,0): 2, 4, 5")
      assert(m.filterNeighbors(pos(1, 1), _ > 0).length == 5, "vecinas de (1,1): 1, 2, 3, 4, 6")
      assert(m.filterNeighbors(pos(1, 1), _ == 5).isEmpty, "la propia celda no cuenta")
    },
    test("(recursión de cola) constructores, get, update y partition con 90.000 elementos") {
      val big = Matrix(300, 300, 1)
      assert(big.get(pos(299, 299)) == 1)
      assert(big.update(pos(299, 299), 7).get(pos(299, 299)) == 7)
      val (yes, no) = big.update(pos(0, 0), 2).partition(_ == 1)
      assert(yes.length == 89999 && no == List(pos(0, 0)))
      assert(Matrix(300, 300, 0, List(pos(299, 299)), 1).get(pos(299, 299)) == 1)
    }
  )
  summary("Matrix", results)
