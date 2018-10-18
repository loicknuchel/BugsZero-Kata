package com.adaptionsoft.games.uglytrivia

import scala.collection.mutable

// this constructor ensure that there is at least two players
class Game(player1: String, player2: String, otherPlayers: String*) {

  import Game._

  private val playerNames = player1 +: player2 +: otherPlayers
  // removing places, purses and inPenaltyBox arrays allows for any number of players
  private val players = playerNames.map(name => Player(name))
  private val questions = Questions(Category.values)
  private var currentPlayerIndex = 0
  private var currentPlayer = players(currentPlayerIndex)

  // will throw an exception if there is too much players
  // it's better than having errors from elsewhere in the program
  // depending on specification, it can be improved by allowing more players or by having different constructors for each number of players
  assert(playerNames.length <= MaxPlayerNumber, s"Game should have $MaxPlayerNumber players but has ${playerNames.length} (${playerNames.mkString(", ")})")

  // print init messages
  Messages.init(players)

  // this method is now useless as there is always two or more players
  def isPlayable: Boolean = true

  def howManyPlayers: Int = players.size

  // temporal coupling: create a single method to avoid errors with call order (roll then answer then roll then answer...)
  // prefer positive Booleans (return hasWon instead of notAWinner), it's easier to manipulate
  // merging all methods here will help refactoring code logic by removing method boundaries
  // flatten if structure by reverting the condition
  // trying to improve similarity make clear that nextPlayer() method in condition isGettingOutOfPenaltyBox is called too soon
  // having all the code in the same place, it highlights that inPenaltyBox is never set to false
  // actions when not in penalty box and when exit penalty box is very similar so merge it
  def play(roll: Int, answeredCorrectly: Boolean): Boolean = {
    Messages.play(currentPlayer, roll)

    val normalPlay = !currentPlayer.inPenaltyBox || canExitPenaltyBox(roll)

    if (normalPlay) {
      if (currentPlayer.inPenaltyBox) Messages.exitPenaltyBox(currentPlayer)
      currentPlayer.move(roll)
      val category = Category.from(currentPlayer.place)
      val question = questions.pick(category)
      Messages.move(currentPlayer, category, question)
    } else {
      Messages.stayInPenaltyBox(currentPlayer)
    }

    if (!answeredCorrectly) {
      currentPlayer.inPenaltyBox = true
      Messages.incorrectAnswer(currentPlayer)
    } else if (normalPlay) {
      currentPlayer.inPenaltyBox = false
      currentPlayer.addGold(1)
      Messages.correctAnswer(currentPlayer)
    }

    val winner = currentPlayer.hasWon
    nextPlayer()
    winner
  }

  // this method will add meaning to the action, it's clearer in the if condition
  private def canExitPenaltyBox(roll: Int): Boolean = roll % 2 == 1

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
  val GoldToWin = Gold(6)
  val NumberOfQuestions = 50

  // add constraints on possible categories and improve types
  sealed trait Category

  object Category {

    case object Pop extends Category

    case object Science extends Category

    case object Sports extends Category

    case object Rock extends Category

    val values: Seq[Category] = Seq(Pop, Science, Sports, Rock)

    def from(place: Place): Category = Category.values(place.value % 4)
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

  // Place and Gold classes improve code types and package some specific behaviour
  case class Place(value: Int) extends AnyVal {
    def +(roll: Int): Place = Place((value + roll) % NumberOfCells)

    override def toString: String = value.toString
  }

  case class Gold(value: Int) extends AnyVal {
    def +(amount: Int): Gold = Gold(value + amount)

    override def toString: String = value.toString
  }

  // group player info inside a class instead of multiple arrays
  case class Player(name: String,
                    var place: Place,
                    var purse: Gold,
                    var inPenaltyBox: Boolean) {
    // adding methods reveals what is the intention and factorize some code
    def move(roll: Int): Unit = place += roll

    def addGold(amount: Int): Unit = purse += amount

    def hasWon: Boolean = purse == GoldToWin
  }

  object Player {
    def apply(name: String): Player = new Player(name, Place(0), Gold(0), false)
  }

  object Messages {
    def init(players: Seq[Player]): Unit = {
      players.zipWithIndex.foreach { case (p, i) =>
        println(s"${p.name} was added")
        println(s"They are player number ${i + 1}")
      }
    }

    def play(player: Player, roll: Int): Unit = {
      println(s"${player.name} is the current player")
      println(s"They have rolled a $roll")
    }

    def move(player: Player, category: Category, question: String): Unit = {
      println(s"${player.name}'s new location is ${player.place}")
      println(s"The category is $category")
      println(question)
    }

    def correctAnswer(player: Player): Unit = {
      println("Answer was correct!!!!")
      println(s"${player.name} now has ${player.purse} Gold Coins.")
    }

    def incorrectAnswer(player: Player): Unit = {
      println("Question was incorrectly answered")
      println(s"${player.name} was sent to the penalty box")
    }

    def exitPenaltyBox(player: Player): Unit = {
      println(s"${player.name} is getting out of the penalty box")
    }

    def stayInPenaltyBox(player: Player): Unit = {
      println(s"${player.name} is not getting out of the penalty box")
    }
  }

}
