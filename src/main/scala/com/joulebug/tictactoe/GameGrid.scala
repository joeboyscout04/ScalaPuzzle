package com.joulebug.tictactoe

import com.joulebug.tictactoe.MoveType.MoveType


/**
 * Created by nmiano on 8/8/15.
 */
object MoveType extends Enumeration {
  type MoveType   = Value
  val X, O, Blank = Value
}

class GameGrid {

  type Grid = List[List[MoveType.Value]]

  var grid:Grid = emptyGrid

  def emptyGrid:Grid = List.fill(3)(List.fill(3)(MoveType.Blank))
  
  def updateGrid(rowCoord: Int, colCoord: Int, move: MoveType.Value): Grid = {
    //store move in array
    //TODO: make sure we don't overwrite a non-blank value
    val row = grid(rowCoord)
    val updatedRow = row.updated(colCoord,move)
    grid.updated(rowCoord,updatedRow)
  }

  /**
   * Checks if tic-tac-toe game contains a winner
   * @return
   */
  def containsWinner:Boolean = {
    //checks if set contains only a single element which is not equals to Blank
    def setHelper(set: Set[MoveType.Value]): Boolean = set.size == 1 && !set.contains(MoveType.Blank)

    //helpers for checking row, column, and diagonal wins
    def rowHelper(n: Int): Boolean = setHelper(grid(n).toSet)

    def colHelper(n: Int): Boolean = setHelper(List(grid(0)(n), grid(1)(n), grid(2)(n)).toSet)

    def diagHelper: Boolean =
      setHelper(List(grid(0)(0), grid(1)(1), grid(2)(2)).toSet) ||
        setHelper(List(grid(0)(2), grid(1)(1), grid(2)(0)).toSet)

    //final check
    rowHelper(0) || rowHelper(1) || rowHelper(2) || colHelper(0) || colHelper(1) || colHelper(2) || diagHelper
  }
}
