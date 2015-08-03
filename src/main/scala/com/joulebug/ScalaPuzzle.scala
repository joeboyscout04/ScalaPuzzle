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

  def startTicTacToe =  {

    println("Player 1:  Choose your Mark (X,O)")
    val player1Input = readLine().toUpperCase()
    var player1Move = MoveType.Blank
    var player2Move = MoveType.Blank
    val playerMoveTypeBox = nonBlankMoves.find(_.toString == player1Input)
    if(playerMoveTypeBox.isDefined) {
        player1Move = playerMoveTypeBox.get
        player2Move = nonBlankMoves.filterNot(_ == player1Move).head
    }
    else {
      println("Incorrect mark type selected!  Please choose X or O.")
    }
    //exit here.


    //start loop
    var moveNumber = 1
    var winnerFound = false
    while(moveNumber <= 9 && !winnerFound){


      //make move
      //store move in array
      //check for winner
      //exit the loop
      //program over
      //makeMove()
    }



  def makeMove(move:MoveType.Value): Unit ={

    println("Please indicate what row,column you want to put the mark")

    //TODO: Error handling
    val coordinates = readLine().split(",")
    val rowCoord = coordinates(0).toInt
    val colCoord = coordinates(1).toInt

    val row = grid(rowCoord)
    val updatedRow = row.updated(colCoord,move)
  }


  def checkForWinner(grid: List[List[MoveType]]):Boolean =
    (grid(0)(0) == grid(0)(1) && grid(0)(0) == grid(0)(2)) ||
      (grid(1)(0) == grid(1)(1) && grid(1)(0) == grid(1)(2)) ||
      (grid(2)(0) == grid(2)(1) && grid(2)(0) == grid(2)(2)) ||
      (grid(0)(0) == grid(1)(1) && grid(0)(0) == grid(2)(2)) ||
      (grid(2)(0) == grid(1)(1) && grid(2)(0) == grid(0)(2))
  }


  val updatedGrid = grid.updated(rowCoord,updatedRow)

  grid = updatedGrid

  def checkForWinner:Boolean = {
    false
  }
}
