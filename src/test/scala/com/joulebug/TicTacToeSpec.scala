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

  "Valid moves" should "be a list of tuples representing the coordinates of valid moves in the game grid" in {
    val game0 = new Game()
    game0.validMoves should be === List((2,2),(2,1),(2,0),(1,2),(1,1),(1,0),(0,2),(0,1),(0,0))

    val game1 = game0.updateGrid(0, 0, MoveType.X)
    game1.validMoves should be === List((2,2),(2,1),(2,0),(1,2),(1,1),(1,0),(0,2),(0,1))

    val game2 = game1.updateGrid(0, 1, MoveType.O)
    game2.validMoves should be === List((2,2),(2,1),(2,0),(1,2),(1,1),(1,0),(0,2))

    val game3 = game2.updateGrid(0, 2, MoveType.X)
    game3.validMoves should be === List((2,2),(2,1),(2,0),(1,2),(1,1),(1,0))

    val game4 = game3.updateGrid(1, 0, MoveType.O)
    game4.validMoves should be === List((2,2),(2,1),(2,0),(1,2),(1,1))

    val game5 = game4.updateGrid(1, 1, MoveType.X)
    game5.validMoves should be === List((2,2),(2,1),(2,0),(1,2))

    val game6 = game5.updateGrid(1, 2, MoveType.O)
    game6.validMoves should be === List((2,2),(2,1),(2,0))

    val game7 = game6.updateGrid(2, 0, MoveType.X)
    game7.validMoves should be === List((2,2),(2,1))

    val game8 = game7.updateGrid(2, 1, MoveType.O)
    game8.validMoves should be === List((2,2))

    val game9 = game8.updateGrid(2, 2, MoveType.X)
    game9.validMoves should be === List()
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
