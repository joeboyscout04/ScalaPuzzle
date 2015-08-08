package com.joulebug

import com.joulebug.tictactoe.{Game, MoveType}

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
    var moveNumber = 1
    var winnerFound = false
    while (moveNumber <= 9 && !winnerFound) {

      val modulo = moveNumber % 2
      var currentPlayer = MoveType.Blank
      println("Move number " + moveNumber + ".")

      if (modulo > 0) currentPlayer = MoveType.X
      else currentPlayer = MoveType.O

      println("Make your move, Player " + currentPlayer.toString)
      randomMove(currentPlayer)

      println("Check for winner")
      winnerFound = game.containsWinner

      if (winnerFound) {
        println("We found a winner!")
        println("The winner was " + currentPlayer.toString + "!")
      }

      moveNumber += 1
    }

    //program over
    println("We're done!  Thanks for playing!")
    game.grid = game.emptyGrid
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
