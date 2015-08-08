package com.joulebug

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
        XDIA1, XDIA2
        = Value
  }

  def quickWinGame(winType: WinType.Value) = {
    val game = TicTacToe

    val xRow = List.fill(3)(MoveType.X)
    val oRow = List.fill(3)(MoveType.O)

    //diag1
    val xDia0Row0 = List(MoveType.X, MoveType.O, MoveType.O)
    val xDia0Row1 = List(MoveType.O, MoveType.X, MoveType.O)
    val xDia0Row2 = List(MoveType.X, MoveType.O, MoveType.X)

    //diag1
    val xDia1Row0 = List(MoveType.O, MoveType.O, MoveType.X)
    val xDia1Row1 = List(MoveType.O, MoveType.X, MoveType.O)
    val xDia1Row2 = List(MoveType.X, MoveType.O, MoveType.X)

    game.grid = winType match {
      case WinType.XROW0 => game.grid.updated(0, xRow)
      case WinType.XROW1 => game.grid.updated(1, xRow)
      case WinType.XROW2 => game.grid.updated(2, xRow)
      case WinType.OROW0 => game.grid.updated(0, oRow)
      case WinType.OROW1 => game.grid.updated(1, oRow)
      case WinType.OROW2 => game.grid.updated(2, oRow)
      case WinType.XDIA1 => game.grid.updated(0, xDia0Row0)
                                     .updated(1, xDia0Row1)
                                     .updated(2, xDia0Row2)
      case WinType.XDIA2 => game.grid.updated(0, xDia1Row0)
                                     .updated(1, xDia1Row1)
                                     .updated(2, xDia1Row2)
      case _             => game.grid.updated(0, oRow)
    }
    game
  }

  "Check for winner with no winner" should "return false" in {
    TicTacToe.checkForWinner() should be === false
  }

  "Check for winner with winner" should "return true" in {
    val gameX0 = quickWinGame(WinType.XROW0)
    gameX0.checkForWinner() should be === true

    val gameX1 = quickWinGame(WinType.XROW1)
    gameX1.checkForWinner() should be === true

    val gameX2 = quickWinGame(WinType.XROW2)
    gameX2.checkForWinner() should be === true

    val gameO0 = quickWinGame(WinType.OROW0)
    gameO0.checkForWinner() should be === true

    val gameO1 = quickWinGame(WinType.OROW1)
    gameX1.checkForWinner() should be === true

    val gameO2 = quickWinGame(WinType.OROW2)
    gameO2.checkForWinner() should be === true

    val gameDiaX0 = quickWinGame(WinType.XDIA1)
    gameDiaX0.checkForWinner() should be === true

    val gameDiaX1 = quickWinGame(WinType.XDIA2)
    gameDiaX1.checkForWinner() should be === true
  }
}
