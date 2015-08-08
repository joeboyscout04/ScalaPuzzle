package com.joulebug

import org.scalatest.FlatSpec
import org.scalatest.Matchers

/**
 * Created by nmiano on 8/8/15.
 */
class TicTacToeSpec extends FlatSpec with Matchers{

  "Check for winner with no winner" should "return false" in {
    TicTacToe.checkForWinner() should be === false
  }
}
