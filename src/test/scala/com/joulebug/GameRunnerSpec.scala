package com.joulebug

import com.joulebug.tictactoe.{MoveType, Game}
import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by nmiano on 8/20/15.
 */
class GameRunnerSpec extends FlatSpec with Matchers with GameFlatSpecUtil {
  "UserMove" should "" in {
    val game = new Game
    val runner = new GameRunner

    val moveOne = runner.randomMove(game, MoveType.X)
    gridToSortedList(moveOne.grid) should be === List(MoveType.X)

    val moveTwo = runner.randomMove(moveOne, MoveType.O)
    gridToSortedList(moveTwo.grid) should be === List(MoveType.X, MoveType.O)

    val moveThree = runner.randomMove(moveTwo, MoveType.X)
    gridToSortedList(moveThree.grid) should be === List(MoveType.X, MoveType.X, MoveType.O)

    val moveFour = runner.randomMove(moveThree, MoveType.O)
    gridToSortedList(moveFour.grid) should be === List(MoveType.X, MoveType.X, MoveType.O, MoveType.O)

    val moveFive = runner.randomMove(moveFour, MoveType.X)
    gridToSortedList(moveFive.grid) should be === List(MoveType.X, MoveType.X, MoveType.X, MoveType.O, MoveType.O)

    val moveSix = runner.randomMove(moveFive, MoveType.O)
    gridToSortedList(moveSix.grid) should be === List(MoveType.X, MoveType.X, MoveType.X, MoveType.O, MoveType.O,
                                                      MoveType.O)

    val moveSeven = runner.randomMove(moveSix, MoveType.X)
    gridToSortedList(moveSeven.grid) should be === List(MoveType.X, MoveType.X, MoveType.X, MoveType.X, MoveType.O,
                                                        MoveType.O, MoveType.O)

    val moveEight = runner.randomMove(moveSeven, MoveType.O)
    gridToSortedList(moveEight.grid) should be === List(MoveType.X, MoveType.X, MoveType.X, MoveType.X, MoveType.O,
                                                        MoveType.O, MoveType.O, MoveType.O)

    val moveNine = runner.randomMove(moveEight, MoveType.X)
    gridToSortedList(moveNine.grid) should be === List(MoveType.X, MoveType.X, MoveType.X, MoveType.X, MoveType.X,
                                                       MoveType.O, MoveType.O, MoveType.O, MoveType.O)

    val moveTen = runner.randomMove(moveNine, MoveType.O)
    gridToSortedList(moveTen.grid) should be === List(MoveType.X, MoveType.X, MoveType.X, MoveType.X, MoveType.X,
                                                      MoveType.O, MoveType.O, MoveType.O, MoveType.O)
  }
}
