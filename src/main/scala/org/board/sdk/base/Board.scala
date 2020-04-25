package org.board.sdk.base

class Board(val squareMatrix: Array[Array[Square]]) {

  def this(charData: Array[Array[Char]], air: Char, rock: Char) = {
    this(charData.zipWithIndex.map(arrIndex => arrIndex._1.zipWithIndex.map(
      symIndex => if (symIndex._1 == air) AirSquare(symIndex._2, arrIndex._2, symIndex._1) else
                                          RockSquare(symIndex._2, arrIndex._2, symIndex._1))))
  }

  def this(rawData: List[String], air: Char, rock: Char) = {
    this(rawData.map(_.toCharArray).toArray, air, rock)
  }

  override def toString: String = {
    (squareMatrix.map(row => row.map(_.sym.toString).reduce(_ + _))).mkString("\n")
  }

}
