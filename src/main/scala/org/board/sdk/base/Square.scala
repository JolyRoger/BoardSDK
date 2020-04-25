package org.board.sdk.base

sealed abstract class Square(val x: Int, val y: Int) {
  def air: Boolean
  def rock: Boolean
  def sym: Char
  var free: Boolean = true
  var opp: Boolean = true
}
case class AirSquare(override val x: Int, override val y: Int, override val sym: Char) extends Square(x, y) {
  override val air = true
  override val rock = false
}
case class RockSquare(override val x: Int, override val y: Int, override val sym: Char) extends Square(x, y) {
  override val air = false
  override val rock = true
}
