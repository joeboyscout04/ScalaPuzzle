package com.joulebug

import com.joulebug.tictactoe.{Game, MoveType}

import org.scalatest.FlatSpec
import org.scalatest.Matchers

/**
 * Created by nmiano on 8/8/15.
 */
class TicTacToeSpec extends FlatSpec with Matchers{

  object WinType extends Enumeration {
    val XROW0, XROW1, XROW2,
        OROW0, OROW1, OROW2,
        XCOL0, XCOL1, XCOL2,
        OCOL0, OCOL1, OCOL2,
        XDIA0, XDIA1,
        ODIA0, ODIA1
        = Value
  }

  "Check for winner with no winner" should "return false" in {
    val losingGame0 = new Game
    losingGame0.containsWinner should be === false
  }

  "Check for winner with winner" should "return true" in {
    val gameX0 = quickWinGame(WinType.XROW0)
    gameX0.containsWinner should be === true

    val gameX1 = quickWinGame(WinType.XROW1)
    gameX1.containsWinner should be === true

    val gameX2 = quickWinGame(WinType.XROW2)
    gameX2.containsWinner should be === true

    val gameO0 = quickWinGame(WinType.OROW0)
    gameO0.containsWinner should be === true

    val gameO1 = quickWinGame(WinType.OROW1)
    gameO1.containsWinner should be === true

    val gameO2 = quickWinGame(WinType.OROW2)
    gameO2.containsWinner should be === true

    val gameXCol0 = quickWinGame(WinType.XCOL0)
    gameXCol0.containsWinner should be === true

    val gameXCol1 = quickWinGame(WinType.XCOL1)
    gameXCol1.containsWinner should be === true

    val gameXCol2 = quickWinGame(WinType.XCOL2)
    gameXCol2.containsWinner should be === true

    val gameOCol0 = quickWinGame(WinType.OCOL0)
    gameOCol0.containsWinner should be === true

    val gameOCol1 = quickWinGame(WinType.OCOL1)
    gameOCol1.containsWinner should be === true

    val gameOCol2 = quickWinGame(WinType.OCOL2)
    gameOCol2.containsWinner should be === true

    val gameDiaX0 = quickWinGame(WinType.XDIA0)
    gameDiaX0.containsWinner should be === true

    val gameDiaX1 = quickWinGame(WinType.XDIA1)
    gameDiaX1.containsWinner should be === true

    val gameDiaO0 = quickWinGame(WinType.ODIA0)
    gameDiaO0.containsWinner should be === true

    val gameDiaO1 = quickWinGame(WinType.ODIA1)
    gameDiaO1.containsWinner should be === true
  }

  /**
   * Ugly helper method for quickly generating wins
   * by WinType.
   * @param winType type of win
   * @return completed game with win type
   */
  private def quickWinGame(winType: WinType.Value) = {
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
}
