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

      val modulo = moveNumber % 2
      println("Move number "+moveNumber+".")
      if(modulo > 0){
        //player 1

        println("Make your move, Player 1")
        makeMove(player1Move)
      }
      else {
        //player 2
        println("Make your move, Player 2")
        makeMove(player2Move)
      }


      //check for winner
      winnerFound = checkForWinner()
      if(winnerFound){
        println("We found a winner!")
      }


      moveNumber += 1

      //exit the loop
      //program over
    }

    //program over
    println("We're done!  Thanks for playing!")


  //make move for the player.
  def makeMove(move:MoveType.Value): Unit ={

    var validMove = false
    var rowCoord = -1
    var colCoord = -1
    while(!validMove) {
      println("Please indicate what row,column you want to put the mark (1,2,3)")

      //TODO: Error handling
      val coordinates = readLine().split(",")
      rowCoord = coordinates(0).toInt - 1
      colCoord = coordinates(1).toInt - 1

      var movePoint = grid(rowCoord)(colCoord)
      if(movePoint == MoveType.Blank){
        validMove = true
      }
      else {
        println("Sorry, that spot is already taken.  Please try again.")
      }
    }

    //store move in array
    //TODO: make sure we don't overwrite a non-blank value
    val row = grid(rowCoord)
    val updatedRow = row.updated(colCoord,move)

    val updatedGrid = grid.updated(rowCoord,updatedRow)

    grid = updatedGrid
    grid.foreach(row=>{
      println(row.toString)
    })
  }

  def checkForWinner():Boolean =

    (grid(0)(0) == grid(0)(1) && grid(0)(0) == grid(0)(2) && grid(0)(0) != MoveType.Blank) ||
      (grid(1)(0) == grid(1)(1) && grid(1)(0) == grid(1)(2) && grid(1)(0) != MoveType.Blank) ||
      (grid(2)(0) == grid(2)(1) && grid(2)(0) == grid(2)(2) && grid(2)(0) != MoveType.Blank) ||
      (grid(0)(0) == grid(1)(1) && grid(0)(0) == grid(2)(2) && grid(0)(0) != MoveType.Blank) ||
      (grid(2)(0) == grid(1)(1) && grid(2)(0) == grid(0)(2) && grid(2)(0) != MoveType.Blank)
  }

}
