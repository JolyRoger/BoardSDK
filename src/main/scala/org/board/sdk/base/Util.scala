package org.board.sdk.base

import scala.io.Source
import scala.util.{Failure, Success, Try, Using}

object Util {

  def readAsList(filename: String): List[String] = {
    readAsTry(filename) match {
      case Success(lines) => lines
      case Failure(e) =>
        Console.err.println(s"${e.getMessage}")
        List.empty
    }
  }

  def readAsTry(filename: String): Try[List[String]] = {
    Using(Source.fromFile(filename)) { _.getLines.toList }
  }

  def linesToBoard(lines: List[String]): Board = {
    new Board(lines, '.', 'x')
  }
}
