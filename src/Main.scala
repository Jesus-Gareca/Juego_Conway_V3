package gameoflife

import gameoflife.Player.Player1

import scala.annotation.tailrec
import scala.io.StdIn
import scala.util.Try

object Main extends App:
  val BoardSize      = 15
  val MaxGenerations = 1000

    // Lee un número entero por teclado que dec+be estar dentro del mínimo y el máximo
    // si no vuelve a preguntar
    def readInt(prompt: String, min: Int, max: Int): Int =
      @tailrec
      def aux(): Int =
        println(prompt)
        Try(StdIn.readLine().toInt).toOption match
          case Some(n) if n >= min && n <= max => n
          case _ =>
            println(s"Introduce un número entre $min y $max.")
            aux()
      aux()

    // Pregunta si es humano delegando en readYesNo
    def readHumanOrAI(prompt: String): Boolean = {
      readYesNo(prompt)
    } //TODO

    // Lee e interpreta una respuesta tipo (s/n), acepta "s", "si" "sí", "n" y "no"
    // cualquier otra cosa vuelve a preguntar
    def readYesNo(prompt: String): Boolean = {
      @tailrec
      def aux(): Boolean = {
        println(prompt)
        Try(StdIn.readLine().trim.toLowerCase()).toOption match
          case Some("s") | Some("si") | Some("sí") => true
          case Some("n") | Some("no") => false
          case _ => println("Debe responder con si o no.")
            aux()
      }
      aux()
    }//TODO

    // Este método genera el mensaje final, si hay ganador mira como ganó, es decir si por
    // generaciones o por número de células. Y si no hay ganador el de empate
    def describeWinner(result: GameResult): String = {
      result.winner match {
        case Some(p) =>
          val (generacionGanador, celVivasGanador) = result.results(p)
          val (generacionPerdedor, celVivasPerdedor) = result.results(otherPlayer(p))
          if (generacionGanador != generacionPerdedor) {
            s"Gana ${playerName(p)} por generaciones ($generacionGanador vs $generacionPerdedor)"
          } else {
            s"Gana ${playerName(p)} por células vivas ($celVivasGanador vs $celVivasPerdedor)"
          }
        case None =>
          val (generacion, celulasVivas) = result.results(Player1)
          s"Empate: mismas generaciones $generacion y mismas células vivas $celulasVivas."
      }
    } //TODO
  println("EL JUEGO DE LA VIDA: la batalla de los patrones")
  // Pide el presupuesto de células y crea la partida
  val num_cel = readInt("¿Cuántas células por jugador? (1-225):", 1, 225)
  val partida = new Game(BoardSize, BoardSize, num_cel, MaxGenerations)
  // Pregunta que es cada jugador si humano o IA
  val jug1 = readHumanOrAI("¿El Jugador 1 es humano? (s/n):")
  val jug2 = readHumanOrAI("¿El Jugador 2 es humano? (s/n):")
  // Asigna la estrategia correspondiente en función de si es humano o IA
  val estrategia1: Strategy = if (jug1) {ManualStrategy} else {AIStrategy(partida, 100)}
  val estrategia2: Strategy = if (jug2) {ManualStrategy} else {AIStrategy(partida, 100)}
  val estrategias: Map[Player, Strategy] = Map(
    Player.Player1 -> estrategia1,
    Player.Player2 -> estrategia2
  )
  // Esto juega la partida, la fase de colocación y simulación
  val resultado = partida.play(estrategias)
  // Muestra los tableros iniciales
  println(resultado.finalState.boards(Player.Player1).show("Inicial Jugador 1"))
  println(resultado.finalState.boards(Player.Player2).show("Inicial Jugador 2"))
  // Muestra las estadísticas de cada jugador
  List(Player.Player1, Player.Player2).foreach { p =>
    val (generacion, celulasVivas) = resultado.results(p)
    println(s"${playerName(p)}: $generacion generaciones, $celulasVivas células vivas")
  }
  // Muestra quién ha ganado y de que forma
  println(describeWinner(resultado))
  // Pregunta si quiere ver la animación de la partida
  if (readYesNo("¿Ver la animación generacion a generación? (s/n):")) {
    Animation.animate(resultado)
  }
   //TODO pedir el numero de celulas a poner, elegir las estrategias, jugar, animar y mostrar el resultado
