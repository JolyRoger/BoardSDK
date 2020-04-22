package org.board.sdk.base

import org.scalatest.{BeforeAndAfter, FlatSpec}
import org.scalactic._
import Tolerance._

class CalcTest extends FlatSpec with BeforeAndAfter {

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
}
