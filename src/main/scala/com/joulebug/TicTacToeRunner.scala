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

  val game = new Game

  def startTicTacToe() {
    //start loop
    gameLoop()
    //program over
    println("We're done!  Thanks for playing!")
    game.grid = game.emptyGrid
  }

  def gameLoop() {
    @tailrec
    def looper(currMove: Int, currPlayer: MoveType.Value, hasWinner: Boolean) {
      if (hasWinner) {
        println("Check for winner")
        println("We found a winner!")
        println("The winner was " + currPlayer.toString + "!")
      } else {
        println("Move number " + currMove + ".")
        //assign player
        val currentPlayer = if (currPlayer == MoveType.O) MoveType.X else MoveType.O

        println("Make your move, Player " + currentPlayer.toString)
        randomMove(currentPlayer)

        looper(currMove + 1, currentPlayer, game.containsWinner)
      }
    }
    looper(0, MoveType.X, game.containsWinner)
  }

  //make move for the player.
  def randomMove(move: MoveType.Value): Unit = {
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

    game.grid = game.updateGrid(rowCoord, colCoord, move)
    game.grid.foreach(row => { println(row.toString) })
  }

  //run game
  startTicTacToe()
}
