package gameoflife

enum Player:
  case Player1, Player2

// Devuelve el rival del jugador dado
def otherPlayer(p: Player): Player = p match
  case Player.Player1 => Player.Player2
  case Player.Player2 => Player.Player1

// Devuelve el nombre del jugador dado
def playerName(p: Player): String = p match
  case Player.Player1 => "Jugador1"
  case Player.Player2 => "Jugador2"
