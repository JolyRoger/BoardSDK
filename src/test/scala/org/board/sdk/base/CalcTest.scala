package org.board.sdk.base

import org.scalatest.BeforeAndAfter
import org.scalactic._
import Tolerance._
import org.board.sdk.base.Direction._
import org.scalatest.flatspec.AnyFlatSpec

class CalcTest extends AnyFlatSpec with BeforeAndAfter {

  "Calc" should "calculate euclidean diastance" in {
    val distTupleDouble = Calc.euclidean((0,0), (1,1))
    val distTupleInt = Calc.euclidean((0,0), (10,0))
    val distSquareDouble = Calc.euclidean((0,0), (4,7))
    val distSquareInt = Calc.euclidean((0,0), (0,6))
    assert(distTupleDouble === 1.414 +- 0.001)
    assert(distTupleInt === 10.0 +- 0.01)
    assert(distSquareDouble === 8.062 +- 0.01)
    assert(distSquareInt === 6.0 +- 0.01)
  }

  "Util" should "read lines from file" in {
    val lines = Util.readAsList("resources/board-15x15.txt")
    val nolines = Util.readAsList("resources/no-such-file.txt")
    assert(lines.size == 15)
    assert(nolines.isEmpty)
  }

  "Calc" should "creat lines structure" in {
    val lines = Util.readAsList("resources/board-15x15.txt")
  }

  "Calc" should "creat board from lines" in {
    val lines = Util.readAsList("resources/board-15x15.txt")
    val board = Util.linesToBoard(lines)
    Console.err.println(s"!!!BOARD:")
    Console.err.println(s"$board")
  }

  "Game" should "take square neighbours" in {
    val cardinal = Set(North, South, West, East)
    val ordinal = Set(NorthWest, SouthWest, SouthEast, NorthEast)
    val windrose = cardinal ++ ordinal

    val lines = Util.readAsList("resources/board-15x15.txt")
    val board = Util.linesToBoard(lines)
    val game = new Game(board)
    val squares = game.neighboursWithDirectionWhile(8,8, windrose, 10, _.isInstanceOf[AirSquare])
    Console.err.println(s"$squares")
    board.show(squares.map(_._1))
  }

  "Game" should "do dfs" in {
    val lines = Util.readAsList("resources/board-15x15.txt")
    val board = Util.linesToBoard(lines)
    val game = new Game(board)
    Console.err.println(s"BFS")
    val reachableSquaresBfs = game.bfs(board(2)(0))
    board.show(reachableSquaresBfs)
    Console.err.println(s"$reachableSquaresBfs")

    Console.err.println(s"DFS")
    val reachableSquaresDfs = game.dfs(board(2)(0))
    board.show(reachableSquaresDfs)
    Console.err.println(s"$reachableSquaresDfs")
  }
}
