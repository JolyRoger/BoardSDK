package org.board.sdk.base

import Direction._
import scala.util.Try

class Game(board: Board) {

  val cardinal = Set(North, South, West, East)
  val ordinal = Set(NorthWest, SouthWest, SouthEast, NorthEast)
  val windrose = cardinal ++ ordinal
  implicit val defaultStep: Int = 1
  implicit val defaultDirection: Set[Direction] = cardinal
  implicit val defaultValid: Square => Boolean = _ => true

  def takeRawSquareTry(square: Square, direction: Direction, step: Int) = {
    direction match {
      case North => Try(board(square.x)(square.y - step))
      case South => Try(board(square.x)(square.y + step))
      case West => Try(board(square.x + 1)(square.y))
      case East => Try(board(square.x - 1)(square.y))
      case NorthWest => Try(board(square.x - step)(square.y - step))
      case SouthWest => Try(board(square.x - step)(square.y + step))
      case NorthEast => Try(board(square.x + step)(square.y - step))
      case SouthEast => Try(board(square.x + step)(square.y + step))
    }
  }

  def getSquaresWithDirection(currentSquare: Square, directions: Set[Direction], step: Int, valid: Square => Boolean) = {

  }

  def getSquares(currentSquare: Square, directions: Set[Direction], step: Int, valid: Square => Boolean) = {
    directions.flatMap { direction =>
      (for (i <- 1 to step) yield takeRawSquareTry(currentSquare, direction, i)).collect {
        case ts if ts.isSuccess => (ts.get, direction)
      }.toSet.filter(s => valid(s._1))
    }
  }

  val collector = getSquares _

  def neighbours(x: Int, y: Int, directions: Set[Direction], step: Int, valid: Square => Boolean) = getSquares(board(x)(y), directions, step, valid)
  def neighbours(x: Int, y: Int, step: Int, valid: Square => Boolean) = getSquares(board(x)(y), defaultDirection, step, valid)
  def neighbours(x: Int, y: Int, directions: Set[Direction], valid: Square => Boolean) = getSquares(board(x)(y), directions, defaultStep, valid)
  def neighbours(x: Int, y: Int, directions: Set[Direction], step: Int) = getSquares(board(x)(y), directions, step, defaultValid)
  def neighbours(x: Int, y: Int, valid: Square => Boolean) = getSquares(board(x)(y), defaultDirection, defaultStep, valid)
  def neighbours(x: Int, y: Int, step: Int) = getSquares(board(x)(y), defaultDirection, step, defaultValid)
  def neighbours(x: Int, y: Int, directions: Set[Direction]) = getSquares(board(x)(y), directions, defaultStep, defaultValid)
  def neighbours(x: Int, y: Int) = getSquares(board(x)(y), defaultDirection, defaultStep, defaultValid)

  def cardinalNeighbours(x: Int, y: Int) = ???
}