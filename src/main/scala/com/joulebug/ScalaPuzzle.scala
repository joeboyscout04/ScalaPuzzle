package com.joulebug

import com.joulebug.MoveType.MoveType


/**
 * Created by josephelliott on 8/3/15.
 */

object MoveType extends Enumeration {
  type MoveType = Value
  val X, O, Blank = Value
}

object TicTacToe {


  val blankList = List.fill(3)(MoveType.Blank)
  var grid:List[List[MoveType.Value]] = List.fill(3)(blankList)

  //create a tic-tac-toe game
  //have computer play itself
  //store the results (file IO)
  //print the resulting winners
  //learn not to play

  val nonBlankMoves = MoveType.values.filterNot(_ == MoveType.Blank)

  def startTicTacToe = {

    //start loop
    var moveNumber = 1
    var winnerFound = false
    while (moveNumber <= 9 && !winnerFound) {

      val modulo = moveNumber % 2
      var currentPlayer = MoveType.Blank
      println("Move number " + moveNumber + ".")
      if (modulo > 0) {
        //player 1
        currentPlayer = MoveType.X
      }
      else {
        //player 2
        currentPlayer = MoveType.O
      }

      println("Make your move, Player " + currentPlayer.toString)
      makeMove(currentPlayer)
      //check for winner
      winnerFound = checkForWinner()
      if (winnerFound) {
        println("We found a winner!")
        println("The winner was " + currentPlayer.toString + "!")
      }

      moveNumber += 1
    }

    //program over
    println("We're done!  Thanks for playing!")
  }

  //make move for the player.
  def makeMove(move:MoveType.Value): Unit ={

    var validMove = false
    var rowCoord = -1
    var colCoord = -1

    val xs = List(1,2,3)
    //val validCoords = xs
  }

  def checkForWinner():Boolean = {

    (grid(0)(0) == grid(0)(1) && grid(0)(0) == grid(0)(2) && grid(0)(0) != MoveType.Blank) ||
      (grid(1)(0) == grid(1)(1) && grid(1)(0) == grid(1)(2) && grid(1)(0) != MoveType.Blank) ||
      (grid(2)(0) == grid(2)(1) && grid(2)(0) == grid(2)(2) && grid(2)(0) != MoveType.Blank) ||
      (grid(0)(0) == grid(1)(1) && grid(0)(0) == grid(2)(2) && grid(0)(0) != MoveType.Blank) ||
      (grid(0)(1) == grid(1)(1) && grid(0)(1) == grid(2)(1) && grid(0)(1) != MoveType.Blank) ||
      (grid(2)(0) == grid(1)(1) && grid(2)(0) == grid(0)(2) && grid(2)(0) != MoveType.Blank)
  }

}
