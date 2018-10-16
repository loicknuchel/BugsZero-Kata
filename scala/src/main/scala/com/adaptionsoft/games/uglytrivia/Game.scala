package com.adaptionsoft.games.uglytrivia

import scala.collection.mutable

class Game(playerNames: Seq[String]) {

  import Game._

  assert(playerNames.size <= MaxNumberOfPlayers, s"At max $MaxNumberOfPlayers players")
  assert(playerNames.size >= MinNumberOfPlayers, s"At least $MinNumberOfPlayers players")

  private val players = playerNames.zipWithIndex.map { case (name, index) => buildPlayer(name, index) }
  private var currentPlayerIndex = 0
  private var currentPlayer = players(currentPlayerIndex)
  private var isGettingOutOfPenaltyBox: Boolean = false
  private val questions: Map[Category, mutable.ListBuffer[String]] =
    Category.values.map(c => c -> mutable.ListBuffer[String]()).toMap

  (0 until NumberOfQuestions).foreach { i =>
    Category.values.foreach(c => questions(c).append(createQuestions(c, i)))
  }

  def isPlayable: Boolean = howManyPlayers >= MinNumberOfPlayers

  def howManyPlayers: Int = players.size

  def roll(roll: Int): Unit = {
    println(s"${currentPlayer.name} is the current player")
    println(s"They have rolled a $roll")
    if (currentPlayer.inPenaltyBox)
      if (roll % 2 != 0) {
        isGettingOutOfPenaltyBox = true
        println(s"${currentPlayer.name} is getting out of the penalty box")
        movePlayerAndAskQuestion(roll)
      } else {
        println(s"${currentPlayer.name} is not getting out of the penalty box")
        isGettingOutOfPenaltyBox = false
      }
    else
      movePlayerAndAskQuestion(roll)
  }

  def wasCorrectlyAnswered: Boolean =
    if (currentPlayer.inPenaltyBox) {
      if (isGettingOutOfPenaltyBox) {
        println("Answer was correct!!!!")
        nextPlayer()
        currentPlayer.purse += 1
        println(s"${currentPlayer.name} now has ${currentPlayer.purse.value} Gold Coins.")
        val winner = didPlayerWin
        winner
      } else {
        nextPlayer()
        true
      }
    } else {
      println("Answer was corrent!!!!")
      currentPlayer.purse += 1
      println(s"${currentPlayer.name} now has ${currentPlayer.purse.value} Gold Coins.")
      val winner = didPlayerWin
      nextPlayer()
      winner
    }

  def wrongAnswer: Boolean = {
    println("Question was incorrectly answered")
    println(s"${currentPlayer.name} was sent to the penalty box")
    currentPlayer.inPenaltyBox = true
    nextPlayer()
    true
  }

  private def buildPlayer(playerName: String, index: Int): Player = {
    println(s"$playerName was added")
    println(s"They are player number ${index + 1}")
    Player(playerName, 0, Gold(0), inPenaltyBox = false)
  }

  private def createQuestions(category: Category, i: Int): String = s"$category Question " + i

  private def nextPlayer(): Unit = {
    currentPlayerIndex += 1
    if (currentPlayerIndex == players.size) currentPlayerIndex = 0
    currentPlayer = players(currentPlayerIndex)
  }

  private def movePlayerAndAskQuestion(roll: Int): Unit = {
    currentPlayer.place = (currentPlayer.place + roll) % MaxNumberOfCells
    println(s"${currentPlayer.name}'s new location is ${currentPlayer.place}")
    println("The category is " + currentCategory)
    askQuestion()
  }

  private def askQuestion(): Unit = {
    println(questions(currentCategory).remove(0))
  }

  private def currentCategory: Category = {
    currentPlayer.place % 4 match {
      case 0 => Category.Pop
      case 1 => Category.Science
      case 2 => Category.Sports
      case _ => Category.Rock
    }
  }

  private def didPlayerWin: Boolean = !(currentPlayer.purse.value == MaxNumberOfPlayers)
}

object Game {
  val MaxNumberOfPlayers = 6
  val MinNumberOfPlayers = 2
  val MaxNumberOfCells = 12
  val NumberOfQuestions = 50

  case class Gold(var value: Int) {
    def +(v: Int): Gold = Gold(this.value + v)
  }

  case class Player(name: String,
                    var place: Int,
                    var purse: Gold,
                    var inPenaltyBox: Boolean)

  sealed trait Category

  object Category {

    case object Pop extends Category

    case object Science extends Category

    case object Sports extends Category

    case object Rock extends Category

    val values = Seq(Pop, Science, Sports, Rock)
  }

}
