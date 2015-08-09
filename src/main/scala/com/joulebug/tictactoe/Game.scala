package com.joulebug.tictactoe

import com.joulebug.tictactoe.MoveType.MoveType


/**
 * Created by nmiano on 8/8/15.
 */
object MoveType extends Enumeration {
  type MoveType   = Value
  val X, O, Blank = Value
}

class Game {

  type Grid = List[List[MoveType.Value]]

  private var _grid:Grid = emptyGrid

  def emptyGrid:Grid = List.fill(3)(List.fill(3)(MoveType.Blank))

  def grid  = _grid

  def updateGrid(rowCoord: Int, colCoord: Int, move: MoveType.Value): Game = {
    //store move in array
    //TODO: make sure we don't overwrite a non-blank value
    val row = _grid(rowCoord)
    val updatedRow = row.updated(colCoord,move)
    _grid = _grid.updated(rowCoord,updatedRow)
    this
  }

  def reset: Grid = {
    _grid = emptyGrid
    _grid
  }

  /**
   * Checks if tic-tac-toe game contains a winner
   * @return
   */
  def containsWinner:Boolean = {
    //checks if set contains only a single element which is not equals to Blank
    def setHelper(set: Set[MoveType.Value]): Boolean = set.size == 1 && !set.contains(MoveType.Blank)

    //helpers for checking row, column, and diagonal wins
    def rowHelper(n: Int): Boolean = setHelper(_grid(n).toSet)

    def colHelper(n: Int): Boolean = setHelper(List(_grid(0)(n), _grid(1)(n), _grid(2)(n)).toSet)

    def diagHelper: Boolean =
      setHelper(List(_grid(0)(0), _grid(1)(1), _grid(2)(2)).toSet) ||
        setHelper(List(_grid(0)(2), _grid(1)(1), _grid(2)(0)).toSet)

    //final check
    rowHelper(0) || rowHelper(1) || rowHelper(2) || colHelper(0) || colHelper(1) || colHelper(2) || diagHelper
  }
}
