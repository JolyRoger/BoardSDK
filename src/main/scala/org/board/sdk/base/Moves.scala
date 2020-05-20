package org.board.sdk.base

import org.board.sdk.base.Direction.Direction

import scala.collection.mutable

trait Moves extends AbstractGame {

  /**
   * Returns all squares reachable in exact number of moves
   * @param square initial square from what search will be performed
   * @param depth how deep search will be performed
   * @return set of squares reachable from {@code square} in {@code depth} moves
   */
  def reachableFrom(square: Square, depth: Int) = {
    square.num = 0
    var stack = List(square)
    var out = Set.empty[Square]
    val marked = mutable.Set.empty[Square]

    while (stack.nonEmpty) {
      val e = stack.head
      marked += e
      val neighbours = cardinal.map(takeRawSquareTry(e.x, e.y, _, 1)).collect {
        case res if res.isSuccess => res.get
      }.filterNot(square => square.rock || marked.contains(square))
      Console.err.println(s"for $e neighbours are $neighbours")
      neighbours.foreach(_.num = e.num + 1)
      out ++= neighbours
      stack = neighbours.filter(_.num < depth).toList ::: stack.tail
      Console.err.println(s"\tstack: $stack")
    }
    out
  }
}
