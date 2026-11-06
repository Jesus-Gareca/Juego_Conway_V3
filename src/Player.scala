package gameoflife

enum Player:
  case Player1, Player2

def otherPlayer(p: Player): Player = p match
  case Player.Player1 => Player.Player2
  case Player.Player2 => Player.Player1

def playerName(p: Player): String = p match
  case Player.Player1 => "Jugador1"
  case Player.Player2 => "Jugador2"
