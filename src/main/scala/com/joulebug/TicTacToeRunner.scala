package com.joulebug

import com.joulebug.tictactoe.{Game, MoveType}

import scala.annotation.tailrec
import scala.util.Random

/**
 * create a tic-tac-toe game
 * have computer play itself
 * store the results (file IO)
 * print the resulting winners
 * learn not to play
 * Created by josephelliott on 8/3/15.
 */

object TicTacToeRunner extends App {

  def startTicTacToe() {
    //start loop
    gameLoop()
    //program over
    println("We're done!  Thanks for playing!")
  }

  /**
   * Runs a game of tic-tac-toe
   */
  def gameLoop() {
    @tailrec
    def looper(game: Game, currMove: Int, currPlayer: MoveType.Value) {
      if (game.containsWinner) {
        println(currPlayer.toString + " wins!")
      } else if (currMove == 9) {
        println("It was a draw!")
      } else {
        println("Move number " + currMove + ".")
        //assign player
        val locPlayer = if (currPlayer == MoveType.O) MoveType.X else MoveType.O

        println("Make your move, Player " + locPlayer.toString)

        val updatedGame = randomMove(game, locPlayer)
        updatedGame.grid.foreach(row => { println(row.toString) })
        looper(updatedGame, currMove + 1, locPlayer)
      }
    }
    //run loop
    looper(new Game, 0, MoveType.X)
  }

  /**
   * Make random move from available moves.
   * @param game current game
   * @param move move (X or O)
   * @return updated game
   */
  def randomMove(game: Game, move: MoveType.Value): Game =
    Random.shuffle(game.validMoves).headOption match {
      case Some(coords) => game.updateGrid(coords._1, coords._2, move)
      case _            => game
  }

  //run game
  startTicTacToe()
}
