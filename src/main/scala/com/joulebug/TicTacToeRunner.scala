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
        println("Check for winner")
        println("We found a winner!")
        println("The winner was " + currPlayer.toString + "!")
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

  //make move for the player.
  def randomMove(game: Game, move: MoveType.Value): Game = {
    var rowCoord = -1
    var colCoord = -1

    //list the valid coordinates
    var validCoordinates:List[(Int,Int)] = List()

    var rowNum = 0

    game.grid.foreach(row => {
      var colNum = 0
      row.foreach(col=>{
        if(col == MoveType.Blank) {
          validCoordinates +:= (rowNum,colNum)
        }
        colNum += 1
      })
      rowNum += 1
    })

    //pick a random one from the valid list.
    val randomCoord = Random.shuffle(validCoordinates).headOption
    if(randomCoord.isDefined) {
      rowCoord = randomCoord.get._1
      colCoord = randomCoord.get._2
    } else {
      println("Something went wrong!")
    }

    game.updateGrid(rowCoord, colCoord, move)
  }

  //run game
  startTicTacToe()
}
