package org.board.sdk.base

class Board(val squareMatrix: Array[Array[Square]]) {

  val transposedMatrix = transpose(squareMatrix)

  def this(charData: Array[Array[Char]], air: Char, rock: Char) = {
    this(charData.zipWithIndex.map(arrIndex => arrIndex._1.zipWithIndex.map(
      symIndex => if (symIndex._1 == air) AirSquare(symIndex._2, arrIndex._2, symIndex._1) else
                                          RockSquare(symIndex._2, arrIndex._2, symIndex._1))))
  }

  def this(rawData: List[String], air: Char, rock: Char) = {
    this(rawData.map(_.toCharArray).toArray, air, rock)
  }

  def apply(x: Int): Array[Square] = transposedMatrix(x)

  private def transpose(matrix: Array[Array[Square]]): Array[Array[Square]] = {
    matrix.head.indices.map(i => matrix.map(_(i))).toArray
  }


  def show(squares: Set[Square]): Unit = {
    Console.err.println(s"${toString(squares)}")
  }

  private def toString(squares: Set[Square]): String = {
    squareMatrix.map(row => row.map(s => if (squares.contains(s)) '\u25AE' else s.sym).mkString("")).mkString("\n")
  }

  override def toString: String = {
    squareMatrix.map(row => row.map(_.sym).mkString("")).mkString("\n")
  }

}
