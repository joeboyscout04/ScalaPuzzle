package com.joulebug

import com.joulebug.tictactoe.{Game, MoveType}
import org.scalatest.{Matchers, FlatSpec}

/**
 * Convenience for some utility functions, etc.
 * Created by nmiano on 8/20/15.
 */
trait GameFlatSpecUtil {
  object WinType extends Enumeration {
    val XROW0, XROW1, XROW2,
    OROW0, OROW1, OROW2,
    XCOL0, XCOL1, XCOL2,
    OCOL0, OCOL1, OCOL2,
    XDIA0, XDIA1,
    ODIA0, ODIA1
    = Value
  }

  /**
   * Ugly helper method for quickly generating wins
   * by WinType.
   * @param winType type of win
   * @return completed game with win type
   */
  def quickWinGame(winType: WinType.Value) = {
    //hacky helper
    def winningGame(coords: List[(Int, Int)], move: MoveType.Value) =
      (new Game).updateGrid(coords(0)._1, coords(0)._2, MoveType.X)
        .updateGrid(coords(1)._1, coords(1)._2, MoveType.X)
        .updateGrid(coords(2)._1, coords(2)._2, MoveType.X)

    //set game grid to win based on win type
    winType match {
      case WinType.XROW0 => winningGame(List((0,0),(0,1),(0,2)), MoveType.X)
      case WinType.XROW1 => winningGame(List((1,0),(1,1),(1,2)), MoveType.X)
      case WinType.XROW2 => winningGame(List((2,0),(2,1),(2,2)), MoveType.X)
      case WinType.OROW0 => winningGame(List((0,0),(0,1),(0,2)), MoveType.O)
      case WinType.OROW1 => winningGame(List((1,0),(1,1),(1,2)), MoveType.O)
      case WinType.OROW2 => winningGame(List((2,0),(2,1),(2,2)), MoveType.O)
      case WinType.XDIA0 => winningGame(List((0,0),(1,1),(2,2)), MoveType.X)
      case WinType.XDIA1 => winningGame(List((0,2),(1,1),(2,0)), MoveType.X)
      case WinType.ODIA0 => winningGame(List((0,0),(1,1),(2,2)), MoveType.O)
      case WinType.ODIA1 => winningGame(List((0,2),(1,1),(2,0)), MoveType.O)
      case WinType.XCOL0 => winningGame(List((0,0),(1,0),(2,0)), MoveType.X)
      case WinType.XCOL1 => winningGame(List((0,1),(1,1),(2,1)), MoveType.X)
      case WinType.XCOL2 => winningGame(List((0,2),(1,2),(2,2)), MoveType.X)
      case WinType.OCOL0 => winningGame(List((0,0),(1,0),(2,0)), MoveType.O)
      case WinType.OCOL1 => winningGame(List((0,1),(1,1),(2,1)), MoveType.O)
      case WinType.OCOL2 => winningGame(List((0,2),(1,2),(2,2)), MoveType.O)
    }
  }

  def gridToSortedList(grid: List[List[MoveType.Value]]): List[MoveType.Value] =
    grid.flatMap(_.filter(_ != MoveType.Blank)).sorted
}
