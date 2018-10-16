package com.adaptionsoft.games.uglytrivia

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Game{
  val MaxNumberOfPlayers = 6
  val MinNumberOfPlayers = 2
  val MaxNumberOfCells = 12
  val NumberOfQuestions = 50
}

case class Gold(var value: Int){
  def += (v: Int) = this.value += v

  override def toString: String = value.toString
}

class Player(val name: String) {
  var place: Int = 0
  var purse: Gold = Gold(0)
  var inPenaltyBox: Boolean = false

  override def toString: String = name
}

sealed trait Category
case object Rock extends Category
case object Pop extends Category
case object Science extends Category
case object Sports extends Category

/**
  * Problems :
  *  x  - forbid to add player when game has started
  *  x  - enforce between 2 and MaxNumberOfPlayer players
  *  x - no magic number (6, 50, 2...)
  *   - question category as enum
  *  X- naming
  *     - add => addPlayer
  *   - safely access arrays
  *  X - create player class
  *   - extract business logic
  *   - Map[Category, List[Question]]
  */
class Game(playerNames: Seq[String]) {
  import Game._

  val players = playerNames.zipWithIndex.map{case (name, index) => addPlayer(name, index)}
  val inPenaltyBox = new Array[Boolean](MaxNumberOfPlayers)

  var currentPlayer = 0
 val questions: Map[Category,mutable.ListBuffer[String]] = Map(
   Pop -> mutable.ListBuffer[String](),
   Sports -> mutable.ListBuffer[String](),
    Science -> mutable.ListBuffer[String](),
   Rock -> mutable.ListBuffer[String]()
 )

  var isGettingOutOfPenaltyBox: Boolean = false

  assert(playerNames.size <= MaxNumberOfPlayers, s"At max $MaxNumberOfPlayers players")
  assert(playerNames.size >= MinNumberOfPlayers, s"At least $MinNumberOfPlayers players")

  (0 until NumberOfQuestions).foreach { i =>
    questions(Pop).append("Pop Question " + i)
    questions(Science).append("Science Question " + i)
    questions(Sports).append("Sports Question " + i)
    questions(Rock).append(createRockQuestion(i))
  }

  def createRockQuestion(index: Int): String = "Rock Question " + index

  def isPlayable: Boolean = howManyPlayers >= MinNumberOfPlayers

  private def addPlayer(playerName: String, index: Int): Player = {
    println(playerName + " was added")
    println(s"They are player number ${index + 1}")
    new Player(playerName)
  }

  def howManyPlayers: Int = players.size

  def roll(roll: Int): Unit = {
    println(players(currentPlayer) + " is the current player")
    println("They have rolled a " + roll)
    if (inPenaltyBox(currentPlayer))
      if (roll % 2 != 0) {
        isGettingOutOfPenaltyBox = true
        println(players(currentPlayer) + " is getting out of the penalty box")
        movePlayerAndAskQuestion(roll)
      } else {
        println(players(currentPlayer) + " is not getting out of the penalty box")
        isGettingOutOfPenaltyBox = false
      }
    else
        movePlayerAndAskQuestion(roll)
  }

  private def movePlayerAndAskQuestion(roll: Int): Unit = {
    players(currentPlayer).place = (players(currentPlayer).place + roll) % MaxNumberOfCells
    println(players(currentPlayer) + "'s new location is " + players(currentPlayer).place)
    println("The category is " + currentCategory)
    askQuestion()
  }

  private def askQuestion(): Unit = {
    println(questions(currentCategory).remove(0))
  }

  private def currentCategory: Category = {
    players(currentPlayer).place % 4 match  {
      case 0 => Pop
      case 1 => Science
      case 2 => Sports
      case _ => Rock
    }
  }

  def wasCorrectlyAnswered: Boolean =
    if (inPenaltyBox(currentPlayer)) {
      if (isGettingOutOfPenaltyBox) {
        println("Answer was correct!!!!")
        currentPlayer += 1
        if (currentPlayer == players.size) currentPlayer = 0
        players(currentPlayer).purse += 1
        println(players(currentPlayer) + " now has " + players(currentPlayer).purse + " Gold Coins.")
        val winner = didPlayerWin
        winner
      } else {
        currentPlayer += 1
        if (currentPlayer == players.size) currentPlayer = 0
        true
      }
    } else {
      println("Answer was corrent!!!!")
      players(currentPlayer).purse += 1
      println(players(currentPlayer) + " now has " + players(currentPlayer).purse + " Gold Coins.")
      val winner = didPlayerWin
      currentPlayer += 1
      if (currentPlayer == players.size) currentPlayer = 0
      winner
    }

  def wrongAnswer: Boolean = {
    println("Question was incorrectly answered")
    println(players(currentPlayer) + " was sent to the penalty box")
    inPenaltyBox(currentPlayer) = true
    currentPlayer += 1
    if (currentPlayer == players.size) currentPlayer = 0
    true
  }


  private def didPlayerWin: Boolean = !(players(currentPlayer).purse.value == MaxNumberOfPlayers)
}
