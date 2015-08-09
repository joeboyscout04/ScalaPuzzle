package com.joulebug.tictactoe

import com.joulebug.tictactoe.MoveType.MoveType


/**
 * Created by nmiano on 8/8/15.
 */
class Game(_grid:List[List[MoveType.Value]] = List.fill(3)(List.fill(3)(MoveType.Blank))) {

  type Grid = List[List[MoveType.Value]]

  def grid  = _grid

  /**
   * Creates copy of game with updated grid.
   * @param rowCoord
   * @param colCoord
   * @param move
   * @return
   */
  def updateGrid(rowCoord: Int, colCoord: Int, move: MoveType.Value): Game = {
    //store move in array
    //TODO: make sure we don't overwrite a non-blank value
    val row        = _grid(rowCoord)
    val updatedRow = row.updated(colCoord,move)
    new Game(_grid.updated(rowCoord,updatedRow))
  }

  /**
   * Creates new game
   * @return
   */
  def reset: Game = new Game

  /**
   * List of coordinates of valid moves.
   * @return
   */
  def validMoves: List[(Int, Int)] = {
    for {
      row <- 0 until _grid.size
      col <- 0 until _grid(row).size
      if _grid(row)(col) == MoveType.Blank
    } yield (row, col)
  }.toList

  /**
   * Checks if tic-tac-toe game contains a winner
   * @return
   */
  def containsWinner:Boolean = {
    //checks if set contains only a single element which is not equals to Blank
    def setHelper(set: Set[MoveType.Value]): Boolean = set.size == 1 && !set.contains(MoveType.Blank)

    //helpers for checking row, column, and diagonal wins
    def rowHelper(n: Int): Boolean = setHelper(_grid(n).toSet)

    def colHelper(n: Int): Boolean = setHelper(List(_grid.head(n), _grid(1)(n), _grid(2)(n)).toSet)

    def diagHelper: Boolean =
      setHelper(List(_grid.head.head, _grid(1)(1), _grid(2)(2)).toSet) ||
        setHelper(List(_grid.head(2), _grid(1)(1), _grid(2).head).toSet)

    //final check
    rowHelper(0) || rowHelper(1) || rowHelper(2) || colHelper(0) || colHelper(1) || colHelper(2) || diagHelper
  }
}

object MoveType extends Enumeration {
  type MoveType   = Value
  val X, O, Blank = Value
}
