package gameoflife

//utilidades para los tests
private val ok   = "\u001b[92m[OK]\u001b[0m"
private val fail = "\u001b[91m[XX]\u001b[0m"

def test(name: String)(body: => Unit): Boolean =
  try
    body
    println(s"  $ok $name")
    true
  catch
    case _: NotImplementedError => println(s"  $fail $name -> todavía sin implementar (???)"); false
    case _: StackOverflowError  => println(s"  $fail $name -> StackOverflowError (¿recursión de cola?)"); false
    case e: Throwable           => println(s"  $fail $name -> ${e.getClass.getSimpleName}: ${e.getMessage}"); false

def summary(suite: String, results: List[Boolean]): Unit =
  println(s"\n$suite: ${results.count(identity)} de ${results.length} pruebas superadas.")

def pos(row: Int, column: Int): Position = Position(row, column)

//Todas las posiciones de un tablero rows x columns, en orden de filas
def allPositions(rows: Int, columns: Int): List[Position] =
  List.tabulate(rows * columns)(i => Position(i / columns, i % columns))

//Construye un tablero directamente a partir de las posiciones vivas (no depende de vuestro update)
def boardWith(rows: Int, columns: Int, alive: List[Position]): GameOfLifeMatrix =
  GameOfLifeMatrix(rows, columns,
    List.tabulate(rows * columns)(i => if alive.contains(Position(i / columns, i % columns)) then Alive else Dead))

def emptyBoard(rows: Int, columns: Int): GameOfLifeMatrix = boardWith(rows, columns, Nil)

//Dibuja un tablero (para los mensajes de error de las pruebas)
def drawBoard(b: GameOfLifeMatrix): String = "\n" + b.show()

def throws[E <: Throwable](body: => Any)(using ct: scala.reflect.ClassTag[E]): Boolean =
  try
    body
    false
  catch
    case e: NotImplementedError => throw e
    case e: Throwable           => ct.runtimeClass.isInstance(e)
