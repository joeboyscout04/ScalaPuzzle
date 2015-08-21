package com.joulebug.tictactoe

import com.joulebug.GameFlatSpecUtil
import org.scalatest.{Matchers, FlatSpec}

/**
 * Created by nmiano on 8/8/15.
 */
class GameSpec extends FlatSpec with Matchers with GameFlatSpecUtil {

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
    game0.validMoves should be === List((0,0),(0,1),(0,2),(1,0),(1,1),(1,2),(2,0),(2,1),(2,2))

    val game1 = game0.updateGrid(0, 0, MoveType.X)
    game1.validMoves should be === List((0,1),(0,2),(1,0),(1,1),(1,2),(2,0),(2,1),(2,2))

    val game2 = game1.updateGrid(0, 1, MoveType.O)
    game2.validMoves should be === List((0,2),(1,0),(1,1),(1,2),(2,0),(2,1),(2,2))

    val game3 = game2.updateGrid(0, 2, MoveType.X)
    game3.validMoves should be === List((1,0),(1,1),(1,2),(2,0),(2,1),(2,2))

    val game4 = game3.updateGrid(1, 0, MoveType.O)
    game4.validMoves should be === List((1,1),(1,2),(2,0),(2,1),(2,2))

    val game5 = game4.updateGrid(1, 1, MoveType.X)
    game5.validMoves should be === List((1,2),(2,0),(2,1),(2,2))

    val game6 = game5.updateGrid(1, 2, MoveType.O)
    game6.validMoves should be === List((2,0),(2,1),(2,2))

    val game7 = game6.updateGrid(2, 0, MoveType.X)
    game7.validMoves should be === List((2,1),(2,2))

    val game8 = game7.updateGrid(2, 1, MoveType.O)
    game8.validMoves should be === List((2,2))

    val game9 = game8.updateGrid(2, 2, MoveType.X)
    game9.validMoves should be === List()
  }
}
