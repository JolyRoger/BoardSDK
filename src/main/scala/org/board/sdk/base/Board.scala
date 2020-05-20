package org.board.sdk.base

class Board(val squareMatrix: Array[Array[Square]]) {

  val transposedMatrix = transpose(squareMatrix)
  val width = transposedMatrix.length
  val height = squareMatrix.length
  lazy val squares = squareMatrix.flatten.toSet
  lazy val size = squares.size

  def this(charData: Array[Array[Char]], air: Char, rock: Char) = {
    this {
      var index = 0
      charData.zipWithIndex.map(arrIndex => arrIndex._1.zipWithIndex.map(
        symIndex => {
          index += 1
          if (symIndex._1 == air)
            AirSquare(symIndex._2, arrIndex._2, symIndex._1, index) else
            RockSquare(symIndex._2, arrIndex._2, symIndex._1, index)
        }))
    }
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
