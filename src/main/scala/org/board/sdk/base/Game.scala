package org.board.sdk.base

import Direction._
import scala.util.Try

class Game(board: Board) extends Neighbours {

  val cardinal = Set(North, South, West, East)
  val ordinal = Set(NorthWest, SouthWest, SouthEast, NorthEast)
  val windrose = cardinal ++ ordinal

  implicit val defaultStep: Int = 1
  implicit val defaultDirection: Set[Direction] = cardinal
  implicit val defaultValid: Square => Boolean = _ => true

  def takeRawSquareTry(x: Int, y: Int, direction: Direction, step: Int) = {
    direction match {
      case North => Try(board(x)(y - step))
      case South => Try(board(x)(y + step))
      case West => Try(board(x + step)(y))
      case East => Try(board(x - step)(y))
      case NorthWest => Try(board(x - step)(y - step))
      case SouthWest => Try(board(x - step)(y + step))
      case NorthEast => Try(board(x + step)(y - step))
      case SouthEast => Try(board(x + step)(y + step))
    }
  }

  private def collectSquares(x: Int, y: Int, direction: Direction, step: Int, valid: Square => Boolean) = {
    (for (i <- 1 to step) yield takeRawSquareTry(x, y, direction, i)).collect {
      case ts if ts.isSuccess => ts.get
    }
  }

  private def collectSquaresWithDirection(x: Int, y: Int, direction: Direction, step: Int, valid: Square => Boolean) = {
    (for (i <- 1 to step) yield takeRawSquareTry(x, y, direction, i)).collect {
      case ts if ts.isSuccess => (ts.get, direction)
    }
  }

  def getSquaresWhile(x: Int, y: Int, directions: Set[Direction], step: Int, valid: Square => Boolean) = {
    directions.flatMap { direction =>
      collectSquares(x, y, direction, step, valid)
        .takeWhile(square => valid(square))
    }
  }

  def getSquaresWithDirectionWhile(x: Int, y: Int, directions: Set[Direction], step: Int, valid: Square => Boolean) = {
    directions.flatMap { direction =>
      collectSquares(x, y, direction, step, valid)
        .takeWhile(square => valid(square))
    }
  }

  def getSquares(x: Int, y: Int, directions: Set[Direction], step: Int, valid: Square => Boolean) = {
    directions.flatMap { direction =>
      collectSquares(x, y, direction, step, valid)
        .toSet.filter(valid(_))
    }
  }

  def getSquaresWithDirection(x: Int, y: Int, directions: Set[Direction], step: Int, valid: Square => Boolean) = {
    directions.flatMap { direction =>
      (for (i <- 1 to step) yield takeRawSquareTry(x, y, direction, i)).collect {
        case ts if ts.isSuccess => (ts.get, direction)
      }.toSet.filter(s => valid(s._1))
    }
  }

  def neighboursWithDirection(x: Int, y: Int, directions: Set[Direction], step: Int, valid: Square => Boolean) = getSquaresWithDirection(x, y, directions, step, valid)
  def neighboursWithDirection(x: Int, y: Int, step: Int, valid: Square => Boolean) = getSquaresWithDirection(x, y, defaultDirection, step, valid)
  def neighboursWithDirection(x: Int, y: Int, directions: Set[Direction], valid: Square => Boolean) = getSquaresWithDirection(x, y, directions, defaultStep, valid)
  def neighboursWithDirection(x: Int, y: Int, directions: Set[Direction], step: Int) = getSquaresWithDirection(x, y, directions, step, defaultValid)
  def neighboursWithDirection(x: Int, y: Int, valid: Square => Boolean) = getSquaresWithDirection(x, y, defaultDirection, defaultStep, valid)
  def neighboursWithDirection(x: Int, y: Int, step: Int) = getSquaresWithDirection(x, y, defaultDirection, step, defaultValid)
  def neighboursWithDirection(x: Int, y: Int, directions: Set[Direction]) = getSquaresWithDirection(x, y, directions, defaultStep, defaultValid)
  def neighboursWithDirection(x: Int, y: Int) = getSquaresWithDirection(x, y, defaultDirection, defaultStep, defaultValid)

  def neighbours(x: Int, y: Int, directions: Set[Direction], step: Int, valid: Square => Boolean) = getSquares(x, y, directions, step, valid)
  def neighbours(x: Int, y: Int, step: Int, valid: Square => Boolean) = getSquares(x, y, defaultDirection, step, valid)
  def neighbours(x: Int, y: Int, directions: Set[Direction], valid: Square => Boolean) = getSquares(x, y, directions, defaultStep, valid)
  def neighbours(x: Int, y: Int, directions: Set[Direction], step: Int) = getSquares(x, y, directions, step, defaultValid)
  def neighbours(x: Int, y: Int, valid: Square => Boolean) = getSquares(x, y, defaultDirection, defaultStep, valid)
  def neighbours(x: Int, y: Int, step: Int) = getSquares(x, y, defaultDirection, step, defaultValid)
  def neighbours(x: Int, y: Int, directions: Set[Direction]) = getSquares(x, y, directions, defaultStep, defaultValid)
  def neighbours(x: Int, y: Int) = getSquares(x, y, defaultDirection, defaultStep, defaultValid)

  def neighboursWhile(x: Int, y: Int, directions: Set[Direction], step: Int, valid: Square => Boolean) = getSquaresWhile(x, y, directions, step, valid)
  def neighboursWhile(x: Int, y: Int, step: Int, valid: Square => Boolean) = getSquaresWhile(x, y, defaultDirection, step, valid)
  def neighboursWhile(x: Int, y: Int, directions: Set[Direction], valid: Square => Boolean) = getSquaresWhile(x, y, directions, defaultStep, valid)
  def neighboursWhile(x: Int, y: Int, directions: Set[Direction], step: Int) = getSquaresWhile(x, y, directions, step, defaultValid)
  def neighboursWhile(x: Int, y: Int, valid: Square => Boolean) = getSquaresWhile(x, y, defaultDirection, defaultStep, valid)
  def neighboursWhile(x: Int, y: Int, step: Int) = getSquaresWhile(x, y, defaultDirection, step, defaultValid)
  def neighboursWhile(x: Int, y: Int, directions: Set[Direction]) = getSquaresWhile(x, y, directions, defaultStep, defaultValid)
  def neighboursWhile(x: Int, y: Int) = getSquaresWhile(x, y, defaultDirection, defaultStep, defaultValid)

  def neighboursWithDirectionWhile(x: Int, y: Int, directions: Set[Direction], step: Int, valid: Square => Boolean) = getSquaresWhile(x, y, directions, step, valid)
  def neighboursWithDirectionWhile(x: Int, y: Int, step: Int, valid: Square => Boolean) = getSquaresWhile(x, y, defaultDirection, step, valid)
  def neighboursWithDirectionWhile(x: Int, y: Int, directions: Set[Direction], valid: Square => Boolean) = getSquaresWhile(x, y, directions, defaultStep, valid)
  def neighboursWithDirectionWhile(x: Int, y: Int, directions: Set[Direction], step: Int) = getSquaresWhile(x, y, directions, step, defaultValid)
  def neighboursWithDirectionWhile(x: Int, y: Int, valid: Square => Boolean) = getSquaresWhile(x, y, defaultDirection, defaultStep, valid)
  def neighboursWithDirectionWhile(x: Int, y: Int, step: Int) = getSquaresWhile(x, y, defaultDirection, step, defaultValid)
  def neighboursWithDirectionWhile(x: Int, y: Int, directions: Set[Direction]) = getSquaresWhile(x, y, directions, defaultStep, defaultValid)
  def neighboursWithDirectionWhile(x: Int, y: Int) = getSquaresWhile(x, y, defaultDirection, defaultStep, defaultValid)
}