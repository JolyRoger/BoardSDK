package org.board.sdk.base

import scala.io.Source
import scala.util.{Failure, Success, Using}

object Util {

    def read(filename: String): List[String] = {
      Using(Source.fromFile(filename)) { _.getLines } match {
        case Success(lines) => lines.toList
        case Failure(e) => {
          Console.err.println(s"${e.getMessage}")
          List.empty
        }
      }
    }
}
