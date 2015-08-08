package com.joulebug

import com.joulebug.MoveType.MoveType

import scala.util.Random


/**
 * Created by josephelliott on 8/3/15.
 */

object MoveType extends Enumeration {
  type MoveType   = Value
  val X, O, Blank = Value
}

object TicTacToe {

  val blankList = List.fill(3)(MoveType.Blank)
  var grid:List[List[MoveType.Value]] = List.fill(3)(blankList)

  def initializeGrid() {
    grid = List.fill(3)(blankList)
  }

  //create a tic-tac-toe game
  //have computer play itself
  //store the results (file IO)
  //print the resulting winners
  //learn not to play

  val nonBlankMoves = MoveType.values.filterNot(_ == MoveType.Blank)

  def startTicTacToe() {

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
      println("Check for winner")
      winnerFound = containsWinner

      if (winnerFound) {
        println("We found a winner!")
        println("The winner was " + currentPlayer.toString + "!")
      }

      moveNumber += 1
    }

    //program over
    println("We're done!  Thanks for playing!")
    initializeGrid()
  }

  //make move for the player.
  def makeMove(move: MoveType.Value): Unit = {
    var rowCoord = -1
    var colCoord = -1

    //list the valid coordinates
    var validCoordinates:List[(Int,Int)] = List()

    var rowNum = 0

    grid.foreach(row=>{
      var colNum = 0
      row.foreach(col=>{
        if(col == MoveType.Blank){
          validCoordinates +:= (rowNum,colNum)
        }
        colNum +=1
      })
      rowNum +=1
    })

    //pick a random one from the valid list.
    val randomCoord = Random.shuffle(validCoordinates).headOption
    if(randomCoord.isDefined){
      rowCoord = randomCoord.get._1
      colCoord = randomCoord.get._2
    }
    else {
      println("Something went wrong!")
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

  /**
   * Checks if tic-tac-toe game contains a winner
   * @return
   */
  def containsWinner:Boolean = {
    //checks if set contains only a single element which is not equals to Blank
    def setHelper(set: Set[MoveType.Value]): Boolean = set.size == 1 && !set.contains(MoveType.Blank)

    //helpers for checking row, column, and diagonal wins
    def rowHelper(n: Int): Boolean = setHelper(grid(n).toSet)

    def colHelper(n: Int): Boolean = setHelper(List(grid(0)(n), grid(1)(n), grid(2)(n)).toSet)

    def diagHelper: Boolean =
      setHelper(List(grid(0)(0), grid(1)(1), grid(2)(2)).toSet) ||
      setHelper(List(grid(0)(2), grid(1)(1), grid(2)(0)).toSet)

    //final check
    rowHelper(0) || rowHelper(1) || rowHelper(2) || colHelper(0) || colHelper(1) || colHelper(2) || diagHelper
  }
}
