package com.adaptionsoft.games.uglytrivia

import scala.collection.mutable

// this constructor ensure that there is at least two players
class Game(player1: String, player2: String, otherPlayers: String*) {

  import Game._

  private val playerNames = player1 +: player2 +: otherPlayers
  // removing places, purses and inPenaltyBox arrays allows for any number of players
  private val players = playerNames.map(name => Player(name, 0, 0, inPenaltyBox = false))
  private val questions = Questions(Category.values)
  private var currentPlayerIndex = 0
  private var currentPlayer = players(currentPlayerIndex)

  // will throw an exception if there is too much players
  // it's better than having errors from elsewhere in the program
  // depending on specification, it can be improved by allowing more players or by having different constructors for each number of players
  assert(playerNames.length <= MaxPlayerNumber, s"Game should have $MaxPlayerNumber players but has ${playerNames.length} (${playerNames.mkString(", ")})")

  // print init messages
  playerNames.zipWithIndex.foreach { case (name, i) =>
    println(name + " was added")
    println("They are player number " + (i + 1))
  }

  // this method is now useless as there is always two or more players
  def isPlayable: Boolean = true

  def howManyPlayers: Int = players.size

  // temporal coupling: create a single method to avoid errors with call order (roll then answer then roll then answer...)
  // prefer positive Booleans (return hasWon instead of notAWinner), it's easier to manipulate
  // merging all methods here will help refactoring code logic by removing method boundaries
  // flatten if structure by reverting the condition
  def play(roll: Int, answeredCorrectly: Boolean): Boolean = {
    println(currentPlayer.name + " is the current player")
    println("They have rolled a " + roll)
    var isGettingOutOfPenaltyBox: Boolean = false

    if (!currentPlayer.inPenaltyBox) {
      movePlayerAndAskQuestion(roll)
    } else if (canExitPenaltyBox(roll)) {
      isGettingOutOfPenaltyBox = true
      println(currentPlayer.name + " is getting out of the penalty box")
      movePlayerAndAskQuestion(roll)
    } else {
      isGettingOutOfPenaltyBox = false
      println(currentPlayer.name + " is not getting out of the penalty box")
    }

    if (!answeredCorrectly) {
      println("Question was incorrectly answered")
      println(currentPlayer.name + " was sent to the penalty box")
      currentPlayer.inPenaltyBox = true
      nextPlayer()
      false
    } else if (!currentPlayer.inPenaltyBox) {
      println("Answer was corrent!!!!")
      currentPlayer.addGold(1)
      println(currentPlayer.name + " now has " + currentPlayer.purse + " Gold Coins.")
      val winner = currentPlayer.hasWon
      nextPlayer()
      winner
    } else if (isGettingOutOfPenaltyBox) {
      println("Answer was correct!!!!")
      nextPlayer()
      currentPlayer.addGold(1)
      println(currentPlayer.name + " now has " + currentPlayer.purse + " Gold Coins.")
      val winner = currentPlayer.hasWon
      winner
    } else {
      nextPlayer()
      false
    }
  }

  // this method will add meaning to the action, it's clearer in the if condition
  private def canExitPenaltyBox(roll: Int): Boolean = roll % 2 == 1

  private def movePlayerAndAskQuestion(roll: Int): Unit = {
    currentPlayer.move(roll)
    println(currentPlayer.name + "'s new location is " + currentPlayer.place)
    val currentCategory = Category.from(currentPlayer.place)
    println("The category is " + currentCategory)
    println(questions.pick(currentCategory))
  }

  // remove code duplication and add semantic to the action
  private def nextPlayer(): Unit = {
    currentPlayerIndex = (currentPlayerIndex + 1) % players.size
    currentPlayer = players(currentPlayerIndex)
  }

  // didPlayerWin method was wrongly named: it returns false when the player did win !!!
  // having the hasWon method, it can be deleted \o/
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
