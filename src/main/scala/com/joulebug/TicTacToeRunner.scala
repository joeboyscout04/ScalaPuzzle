package com.joulebug

import com.joulebug.tictactoe.{Game, MoveType}

import scala.annotation.tailrec
import scala.util.Random
import scala.io._

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
    println("Let's play Tic-Tac-Toe!\n1) human game\n2) random game")
    gameLoop(StdIn.readLine().toInt)
    println("We're done!  Thanks for playing!")
  }

  /**
   * Runs a game of tic-tac-toe
   */
  def gameLoop(gameType: Int) {
    @tailrec
    def looper(game: Game, currMove: Int, moveFunc: (Game, MoveType.Value) => Game, currPlayer: MoveType.Value) {
      if (game.containsWinner) println(currPlayer.toString + " wins!")
      else if (currMove == 9)  println("It was a draw!")
      else {
        println("Move number " + currMove + ".")
        //assign player
        val locPlayer = if (currPlayer == MoveType.O) MoveType.X else MoveType.O
        println("Make your move, Player " + locPlayer.toString)

        val updatedGame = moveFunc(game, locPlayer)
        updatedGame.grid.foreach(row => { println(row.toString()) })
        looper(updatedGame, currMove + 1, moveFunc, locPlayer)
      }
    }
    val moveFunc: (Game, MoveType.Value) => Game = gameType match {
      case 1 => userMove
      case 2 => randomMove
    }
    //run loop
    looper(new Game, 0, moveFunc, MoveType.X)
  }

  /**
   * Make random move from available moves.
   * @param game current game
   * @param move move (X or O)
   * @return updated game
   */
  def randomMove(game: Game, move: MoveType.Value): Game =
    Random.shuffle(game.validMoves).headOption match {
      case Some((x, y)) => game.updateGrid(x, y, move)
      case _            => game
  }

  /**
   * Make user move.
   * @param move
   */
  @tailrec
  def userMove(game: Game, move:MoveType.Value): Game = {
    println("Make your move! [0-2],[0-2]")
    //TODO: Error handling
    val coordinates = StdIn.readLine().split(",")
    val rowCoord = coordinates(0).toInt
    val colCoord = coordinates(1).toInt

    if (game.validMoves.contains((rowCoord, colCoord))) {
      game.updateGrid(rowCoord, colCoord, move)
    } else {
      println("Sorry. Move already taken")
      userMove(game, move)
    }
  }

  //run game
  startTicTacToe()
}
