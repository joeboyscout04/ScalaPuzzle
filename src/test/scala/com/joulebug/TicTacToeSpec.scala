package com.joulebug

import com.joulebug.tictactoe.{MoveType, Game}

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

    val losingGame1 = quickLoseGame
    losingGame1.containsWinner should be === false
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

    val game = new Game

    val xRow = List.fill(3)(MoveType.X)
    val oRow = List.fill(3)(MoveType.O)

    /*
     * Definitely some redundancy in here.. but whatever,
     * this is easy to reason about
     */

    //diagX0
    val xDia0Row0 = List(MoveType.X, MoveType.O, MoveType.O)
    val xDia0Row1 = List(MoveType.O, MoveType.X, MoveType.O)
    val xDia0Row2 = List(MoveType.X, MoveType.O, MoveType.X)

    //diagX1
    val xDia1Row0 = List(MoveType.O, MoveType.O, MoveType.X)
    val xDia1Row1 = List(MoveType.O, MoveType.X, MoveType.O)
    val xDia1Row2 = List(MoveType.X, MoveType.O, MoveType.X)

    //diagO0
    val oDia0Row0 = List(MoveType.O, MoveType.X, MoveType.X)
    val oDia0Row1 = List(MoveType.O, MoveType.O, MoveType.O)
    val oDia0Row2 = List(MoveType.X, MoveType.O, MoveType.O)

    //diagO1
    val oDia1Row0 = List(MoveType.O, MoveType.X, MoveType.O)
    val oDia1Row1 = List(MoveType.X, MoveType.O, MoveType.O)
    val oDia1Row2 = List(MoveType.O, MoveType.O, MoveType.X)

    //colX0
    val xCol0Row0 = List(MoveType.X, MoveType.X, MoveType.O)
    val xCol0Row1 = List(MoveType.X, MoveType.O, MoveType.O)
    val xCol0Row2 = List(MoveType.X, MoveType.O, MoveType.X)

    //colX1
    val xCol1Row0 = List(MoveType.X, MoveType.X, MoveType.O)
    val xCol1Row1 = List(MoveType.O, MoveType.X, MoveType.O)
    val xCol1Row2 = List(MoveType.X, MoveType.X, MoveType.X)

    //colX2
    val xCol2Row0 = List(MoveType.O, MoveType.X, MoveType.X)
    val xCol2Row1 = List(MoveType.X, MoveType.O, MoveType.X)
    val xCol2Row2 = List(MoveType.X, MoveType.O, MoveType.X)

    //colO0
    val oCol0Row0 = List(MoveType.O, MoveType.X, MoveType.O)
    val oCol0Row1 = List(MoveType.O, MoveType.X, MoveType.O)
    val oCol0Row2 = List(MoveType.O, MoveType.O, MoveType.X)

    //colO1
    val oCol1Row0 = List(MoveType.X, MoveType.O, MoveType.O)
    val oCol1Row1 = List(MoveType.O, MoveType.O, MoveType.O)
    val oCol1Row2 = List(MoveType.X, MoveType.O, MoveType.X)

    //colO2
    val oCol2Row0 = List(MoveType.X, MoveType.X, MoveType.O)
    val oCol2Row1 = List(MoveType.O, MoveType.X, MoveType.O)
    val oCol2Row2 = List(MoveType.O, MoveType.O, MoveType.O)
    
    game.grid = winType match {
      case WinType.XROW0 => game.grid.updated(0, xRow)
      case WinType.XROW1 => game.grid.updated(1, xRow)
      case WinType.XROW2 => game.grid.updated(2, xRow)
      case WinType.OROW0 => game.grid.updated(0, oRow)
      case WinType.OROW1 => game.grid.updated(1, oRow)
      case WinType.OROW2 => game.grid.updated(2, oRow)
      case WinType.XDIA0 => List(xDia0Row0, xDia0Row1, xDia0Row2)
      case WinType.XDIA1 => List(xDia1Row0, xDia1Row1, xDia1Row2)
      case WinType.ODIA0 => List(oDia0Row0, oDia0Row1, oDia0Row2)
      case WinType.ODIA1 => List(oDia1Row0, oDia1Row1, oDia1Row2)
      case WinType.XCOL0 => List(xCol0Row0, xCol0Row1, xCol0Row2)
      case WinType.XCOL1 => List(xCol1Row0, xCol1Row1, xCol1Row2)
      case WinType.XCOL2 => List(xCol2Row0, xCol2Row1, xCol2Row2)
      case WinType.OCOL0 => List(oCol0Row0, oCol0Row1, oCol0Row2)
      case WinType.OCOL1 => List(oCol1Row0, oCol1Row1, oCol1Row2)
      case WinType.OCOL2 => List(oCol2Row0, oCol2Row1, oCol2Row2)
    }
    game
  }

  def quickLoseGame = {
    val game = new Game
    game.grid = List(List(MoveType.X,     MoveType.O,     MoveType.X),
                     List(MoveType.X,     MoveType.Blank, MoveType.O),
                     List(MoveType.Blank, MoveType.O,     MoveType.X))
    game
  }
}
