package com.adaptionsoft.games.uglytrivia

import scala.collection.mutable

// this constructor ensure that there is at least two players
class Game(player1: String, player2: String, otherPlayers: String*) {

  import Game._

  private val playerNames = player1 +: player2 +: otherPlayers
  // removing places, purses and inPenaltyBox arrays allows for any number of players
  private val players = mutable.ListBuffer[Player]()
  private val questions = Questions(Category.values)
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

  // this method is now useless as there is always two or more players
  def isPlayable: Boolean = true

  // add is now private to force adding all players at the beginning and forbid adding players later
  // depending on specification, you can leave it public to allow more players to join when the game has started
  private def add(playerName: String): Boolean = {
    players.append(Player(playerName, 0, 0, inPenaltyBox = false))
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
    println(players(currentPlayer).name + " is the current player")
    println("They have rolled a " + roll)
    if (players(currentPlayer).inPenaltyBox) {
      if (roll % 2 != 0) {
        isGettingOutOfPenaltyBox = true
        println(players(currentPlayer).name + " is getting out of the penalty box")
        movePlayerAndAskQuestion(roll)
      } else {
        println(players(currentPlayer).name + " is not getting out of the penalty box")
        isGettingOutOfPenaltyBox = false
      }
    } else
      movePlayerAndAskQuestion(roll)
  }

  private def movePlayerAndAskQuestion(roll: Int): Unit = {
    players(currentPlayer).move(roll)
    println(players(currentPlayer).name + "'s new location is " + players(currentPlayer).place)
    println("The category is " + currentCategory)
    askQuestion()
  }

  private def askQuestion(): Unit = {
    println(questions.pick(currentCategory))
  }

  private def currentCategory: Category = {
    Category.from(players(currentPlayer).place)
  }

  private def wasCorrectlyAnswered: Boolean =
    if (players(currentPlayer).inPenaltyBox) {
      if (isGettingOutOfPenaltyBox) {
        println("Answer was correct!!!!")
        nextPlayer()
        players(currentPlayer).addGold(1)
        println(players(currentPlayer).name + " now has " + players(currentPlayer).purse + " Gold Coins.")
        val winner = didPlayerWin
        winner
      } else {
        nextPlayer()
        true
      }
    } else {
      println("Answer was corrent!!!!")
      players(currentPlayer).addGold(1)
      println(players(currentPlayer).name + " now has " + players(currentPlayer).purse + " Gold Coins.")
      val winner = didPlayerWin
      nextPlayer()
      winner
    }

  private def wrongAnswer: Boolean = {
    println("Question was incorrectly answered")
    println(players(currentPlayer).name + " was sent to the penalty box")
    players(currentPlayer).inPenaltyBox = true
    nextPlayer()
    true
  }

  // remove code duplication and add semantic to the action
  private def nextPlayer(): Unit = {
    currentPlayer = (currentPlayer + 1) % players.size
  }

  private def didPlayerWin: Boolean = !players(currentPlayer).hasWon
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

  // manage questions for each category
  case class Questions(var questions: Map[Category, mutable.ListBuffer[String]]) {
    def pick(category: Category): String =
      questions(category).remove(0)
  }

  object Questions {
    def apply(categories: Seq[Category]): Questions =
      new Questions(categories.map(c => c -> createQuestions(c)).toMap)

    private def createQuestions(category: Category): mutable.ListBuffer[String] = {
      val list = mutable.ListBuffer[String]()
      (0 until NumberOfQuestions).foreach { i =>
        list.append(s"$category Question $i")
      }
      list
    }
  }

  // group player info inside a class instead of multiple arrays
  case class Player(name: String,
                    var place: Int,
                    var purse: Int,
                    var inPenaltyBox: Boolean) {
    // adding methods reveals what is the intention and factorize some code
    def move(roll: Int): Unit = place = (place + roll) % NumberOfCells

    def addGold(amount: Int): Unit = purse += amount

    def hasWon: Boolean = purse == GoldToWin
  }

}
