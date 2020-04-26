package org.board.sdk.base

import Direction._
import scala.util.Try

class Game(board: Board) {

  val cardinal = Set(North, South, West, East)
  val ordinal = Set(NorthWest, SouthWest, SouthEast, NorthEast)
  val windrose = cardinal ++ ordinal

  def takeRawSquareTry(square: Square, direction: Direction, step: Int) = {
    direction match {
      case North => Try(board(square.y - step)(square.x))
      case South => Try(board(square.y + step)(square.x))
      case West => Try(board(square.y)(square.x - step))
      case East => Try(board(square.y)(square.x + step))
      case NorthWest => Try(board(square.y + step)(square.x - step))
      case SouthWest => Try(board(square.y - step)(square.x - step))
      case NorthEast => Try(board(square.y - step)(square.x + step))
      case SouthEast => Try(board(square.y + step)(square.x + step))
    }
  }

  def neighbours(x: Int, y: Int) = board(x)(y)

  def cardinalNeighbours(x: Int, y: Int) = ???
}