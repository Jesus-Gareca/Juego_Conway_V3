package gameoflife

//¡No tocar!
object Animation:

  def clearScreen(): Unit =
    print("\u001b[H\u001b[2J\u001b[3J")
    Console.flush()

  def drawFrame(frame: String): Unit =
    print("\u001b[H" + frame.replace("\n", "\u001b[K\n") + "\u001b[K\n\u001b[J")
    Console.flush()

  def sideBySide(left: String, right: String, gap: Int = 4, minWidth: Int = 0): String =
    val ls    = left.split("\n").toList
    val rs    = right.split("\n").toList
    val width = math.max(minWidth, ls.map(visibleLength).max)
    ls.zipAll(rs, "", "").map((l, r) => l + " " * (width - visibleLength(l) + gap) + r).mkString("\n")

  def visibleLength(s: String): Int = s.replaceAll("\u001b\\[[0-9;]*m", "").length

  def frames(board: GameOfLifeMatrix, generations: Int): List[GameOfLifeMatrix] =
    List.iterate(board, generations + 1)(_.nextGeneration)

  def animate(result: GameResult, delayMillis: Int = 120): Unit =
    val players = List(Player.Player1, Player.Player2)
    val framesOf = players.map(p => p -> frames(result.finalState.boards(p), result.results(p)._1)).toMap
    val total    = players.map(p => framesOf(p).length).max
    def title(p: Player, i: Int): String =
      val fs    = framesOf(p)
      val board = fs(math.min(i, fs.length - 1))
      val state = if i >= fs.length - 1 then " · ✔ estabilizado" else ""
      s"${playerName(p)} · generación ${math.min(i, fs.length - 1)} · ${board.aliveCount} vivas$state"
    def board(p: Player, i: Int): String =
      val fs = framesOf(p)
      fs(math.min(i, fs.length - 1)).show(title(p, i))
    val lefts = List.range(0, total).map(i => board(Player.Player1, i))
    val width = lefts.flatMap(_.split("\n")).map(visibleLength).max
    clearScreen()
    List.range(0, total).foreach: i =>
      drawFrame(sideBySide(lefts(i), board(Player.Player2, i), minWidth = width))
      Thread.sleep(delayMillis)
