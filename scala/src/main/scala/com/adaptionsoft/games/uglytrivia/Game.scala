package com.adaptionsoft.games.uglytrivia

import scala.collection.mutable

// this constructor ensure that there is at least two players
class Game(player1: String, player2: String, otherPlayers: String*) {

  import Game._

  private val playerNames = player1 +: player2 +: otherPlayers
  private val players = mutable.ListBuffer[String]()
  private val places = new Array[Int](MaxPlayerNumber)
  private val purses = new Array[Int](MaxPlayerNumber)
  private val inPenaltyBox = new Array[Boolean](MaxPlayerNumber)
  private val popQuestions = mutable.ListBuffer[String]()
  private val scienceQuestions = mutable.ListBuffer[String]()
  private val sportsQuestions = mutable.ListBuffer[String]()
  private val rockQuestions = mutable.ListBuffer[String]()
  private var currentPlayer = 0
  private var isGettingOutOfPenaltyBox: Boolean = false

  // will throw an exception if there is too much players
  // it's better than having errors from elsewhere in the program
  // depending on specification, it can be improved by allowing more players or by having different constructors for each number of players
  assert(playerNames.length <= MaxPlayerNumber, s"Game should have $MaxPlayerNumber players but has ${playerNames.length} (${playerNames.mkString(", ")})")

  // add all players at the beginning
  playerNames.foreach { name =>
    add(name)
  }
  (0 until NumberOfQuestions).foreach { i =>
    popQuestions.append("Pop Question " + i)
    scienceQuestions.append("Science Question " + i)
    sportsQuestions.append("Sports Question " + i)
    rockQuestions.append(createRockQuestion(i))
  }

  private def createRockQuestion(index: Int): String = "Rock Question " + index

  // this method is now useless as there is always two or more players
  def isPlayable: Boolean = true

  // add is now private to force adding all players at the beginning and forbid adding players later
  // depending on specification, you can leave it public to allow more players to join when the game has started
  private def add(playerName: String): Boolean = {
    players.append(playerName)
    places(howManyPlayers) = 0
    purses(howManyPlayers) = 0
    inPenaltyBox(howManyPlayers) = false
    println(playerName + " was added")
    println("They are player number " + players.size)
    true
  }

  def howManyPlayers: Int = players.size

  // temporal coupling: create a single method to avoid errors with call order (roll then answer then roll then answer...)
  // prefer positive Booleans (return hasWon instead of notAWinner), it's easier to manipulate
  def play(roll: Int, answeredCorrectly: Boolean): Boolean = {
    this.roll(roll)
    if (answeredCorrectly) !wasCorrectlyAnswered
    else !wrongAnswer
  }

  private def roll(roll: Int): Unit = {
    println(players(currentPlayer) + " is the current player")
    println("They have rolled a " + roll)
    if (inPenaltyBox(currentPlayer)) if (roll % 2 != 0) {
      isGettingOutOfPenaltyBox = true
      println(players(currentPlayer) + " is getting out of the penalty box")
      movePlayerAndAskQuestion(roll)
    } else {
      println(players(currentPlayer) + " is not getting out of the penalty box")
      isGettingOutOfPenaltyBox = false
    } else
      movePlayerAndAskQuestion(roll)
  }

  private def movePlayerAndAskQuestion(roll: Int): Unit = {
    places(currentPlayer) = places(currentPlayer) + roll
    if (places(currentPlayer) >= NumberOfCells) places(currentPlayer) = places(currentPlayer) - NumberOfCells
    println(players(currentPlayer) + "'s new location is " + places(currentPlayer))
    println("The category is " + currentCategory)
    askQuestion()
  }

  private def askQuestion(): Unit = {
    if (currentCategory == Category.Pop) println(popQuestions.remove(0))
    if (currentCategory == Category.Science) println(scienceQuestions.remove(0))
    if (currentCategory == Category.Sports) println(sportsQuestions.remove(0))
    if (currentCategory == Category.Rock) println(rockQuestions.remove(0))
  }

  private def currentCategory: Category = {
    Category.from(places(currentPlayer))
  }

  private def wasCorrectlyAnswered: Boolean =
    if (inPenaltyBox(currentPlayer)) {
      if (isGettingOutOfPenaltyBox) {
        println("Answer was correct!!!!")
        nextPlayer()
        purses(currentPlayer) += 1
        println(players(currentPlayer) + " now has " + purses(currentPlayer) + " Gold Coins.")
        val winner = didPlayerWin
        winner
      } else {
        nextPlayer()
        true
      }
    } else {
      println("Answer was corrent!!!!")
      purses(currentPlayer) += 1
      println(players(currentPlayer) + " now has " + purses(currentPlayer) + " Gold Coins.")
      val winner = didPlayerWin
      nextPlayer()
      winner
    }

  private def wrongAnswer: Boolean = {
    println("Question was incorrectly answered")
    println(players(currentPlayer) + " was sent to the penalty box")
    inPenaltyBox(currentPlayer) = true
    nextPlayer()
    true
  }

  // remove code duplication and add semantic to the action
  private def nextPlayer(): Unit = {
    currentPlayer = (currentPlayer + 1) % players.size
  }

  private def didPlayerWin: Boolean = !(purses(currentPlayer) == GoldToWin)
}

object Game {
  val MaxPlayerNumber = 6
  val NumberOfCells = 12
  val GoldToWin = 6
  val NumberOfQuestions = 50

  // add constraints on possible categories and improve types
  sealed trait Category

  object Category {

    case object Pop extends Category

    case object Science extends Category

    case object Sports extends Category

    case object Rock extends Category

    val values: Seq[Category] = Seq(Pop, Science, Sports, Rock)

    def from(place: Int): Category = Category.values(place % 4)
  }

}
