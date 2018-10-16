package com.adaptionsoft.games.uglytrivia

import scala.collection.mutable

class Game(playerNames: Seq[String]) {

  import Game._

  assert(playerNames.size <= MaxNumberOfPlayers, s"At max $MaxNumberOfPlayers players")
  assert(playerNames.size >= MinNumberOfPlayers, s"At least $MinNumberOfPlayers players")

  private val players = playerNames.zipWithIndex.map { case (name, index) => buildPlayer(name, index) }
  private var currentPlayerIndex = 0
  private var isGettingOutOfPenaltyBox: Boolean = false
  private val questions: Map[Category, mutable.ListBuffer[String]] =
    Category.values.map(c => c -> mutable.ListBuffer[String]()).toMap

  (0 until NumberOfQuestions).foreach { i =>
    Category.values.foreach(c => questions(c).append(createQuestions(c, i)))
  }

  def isPlayable: Boolean = howManyPlayers >= MinNumberOfPlayers

  def howManyPlayers: Int = players.size

  def roll(roll: Int): Unit = {
    println(players(currentPlayerIndex) + " is the current player")
    println("They have rolled a " + roll)
    if (players(currentPlayerIndex).inPenaltyBox)
      if (roll % 2 != 0) {
        isGettingOutOfPenaltyBox = true
        println(players(currentPlayerIndex) + " is getting out of the penalty box")
        movePlayerAndAskQuestion(roll)
      } else {
        println(players(currentPlayerIndex) + " is not getting out of the penalty box")
        isGettingOutOfPenaltyBox = false
      }
    else
      movePlayerAndAskQuestion(roll)
  }

  def wasCorrectlyAnswered: Boolean =
    if (players(currentPlayerIndex).inPenaltyBox) {
      if (isGettingOutOfPenaltyBox) {
        println("Answer was correct!!!!")
        nextPlayer()
        players(currentPlayerIndex).purse += 1
        println(players(currentPlayerIndex) + " now has " + players(currentPlayerIndex).purse + " Gold Coins.")
        val winner = didPlayerWin
        winner
      } else {
        nextPlayer()
        true
      }
    } else {
      println("Answer was corrent!!!!")
      players(currentPlayerIndex).purse += 1
      println(players(currentPlayerIndex) + " now has " + players(currentPlayerIndex).purse + " Gold Coins.")
      val winner = didPlayerWin
      nextPlayer()
      winner
    }

  def wrongAnswer: Boolean = {
    println("Question was incorrectly answered")
    println(players(currentPlayerIndex) + " was sent to the penalty box")
    players(currentPlayerIndex).inPenaltyBox = true
    nextPlayer()
    true
  }

  private def buildPlayer(playerName: String, index: Int): Player = {
    println(playerName + " was added")
    println(s"They are player number ${index + 1}")
    Player(playerName, 0, Gold(0), inPenaltyBox = false)
  }

  private def createQuestions(category: Category, i: Int): String = s"$category Question " + i

  private def nextPlayer(): Unit = {
    currentPlayerIndex += 1
    if (currentPlayerIndex == players.size) currentPlayerIndex = 0
  }

  private def movePlayerAndAskQuestion(roll: Int): Unit = {
    players(currentPlayerIndex).place = (players(currentPlayerIndex).place + roll) % MaxNumberOfCells
    println(players(currentPlayerIndex) + "'s new location is " + players(currentPlayerIndex).place)
    println("The category is " + currentCategory)
    askQuestion()
  }

  private def askQuestion(): Unit = {
    println(questions(currentCategory).remove(0))
  }

  private def currentCategory: Category = {
    players(currentPlayerIndex).place % 4 match {
      case 0 => Category.Pop
      case 1 => Category.Science
      case 2 => Category.Sports
      case _ => Category.Rock
    }
  }

  private def didPlayerWin: Boolean = !(players(currentPlayerIndex).purse.value == MaxNumberOfPlayers)
}

object Game {
  val MaxNumberOfPlayers = 6
  val MinNumberOfPlayers = 2
  val MaxNumberOfCells = 12
  val NumberOfQuestions = 50

  case class Gold(var value: Int) {
    def +(v: Int): Gold = Gold(this.value + v)

    override def toString: String = value.toString
  }

  case class Player(name: String,
                    var place: Int,
                    var purse: Gold,
                    var inPenaltyBox: Boolean) {
    override def toString: String = name
  }

  sealed trait Category

  object Category {

    case object Pop extends Category

    case object Science extends Category

    case object Sports extends Category

    case object Rock extends Category

    val values = Seq(Pop, Science, Sports, Rock)
  }

}
