package com.adaptionsoft.games.uglytrivia

class Game(player1: String, player2: String, otherPlayers: String*) {

  import Game._

  // TODO: create a GameState case class
  private val questions = Questions(Category.values)
  private val players = (player1 +: player2 +: otherPlayers).map(name => Player(name, 0, Gold(0), inPenaltyBox = false))
  private var currentPlayerIndex = 0
  private var currentPlayer = players(currentPlayerIndex)

  Messages.init(players)

  def isPlayable: Boolean = true

  def howManyPlayers: Int = players.size

  // TODO: simplify business logic
  // return if the player has won
  def play(roll: Int, correctAnswer: Boolean): Boolean = {
    Messages.play(currentPlayer, roll)

    val normalPlay = currentPlayer.notInPenaltyBox || canExitPenaltyBox(roll)

    if (normalPlay) {
      if (currentPlayer.inPenaltyBox) Messages.exitPenaltyBox(currentPlayer)
      currentPlayer.move(roll)
      val category = Category.from(currentPlayer.place)
      val question = questions.pick(category)
      Messages.move(currentPlayer, category, question)
    } else {
      Messages.stayInPenaltyBox(currentPlayer)
    }

    if (!correctAnswer) {
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

  private def nextPlayer(): Unit = {
    currentPlayerIndex = (currentPlayerIndex + 1) % players.size
    currentPlayer = players(currentPlayerIndex)
  }

  private def canExitPenaltyBox(roll: Int): Boolean = roll % 2 == 1
}

object Game {

  val NumberOfCells = 12
  val GoldToWin = 6

  case class Gold(value: Int) {
    def +(v: Int): Gold = Gold(value + v)
  }

  // TODO: remove mutable attributes
  case class Player(name: String,
                    var place: Int,
                    var purse: Gold,
                    var inPenaltyBox: Boolean) {
    def notInPenaltyBox: Boolean = !inPenaltyBox

    def move(roll: Int): Unit = place = (place + roll) % NumberOfCells

    def addGold(amount: Int): Unit = purse += amount

    def hasWon: Boolean = purse.value == GoldToWin
  }

  sealed trait Category

  object Category {

    case object Pop extends Category

    case object Science extends Category

    case object Sports extends Category

    case object Rock extends Category

    val values: Seq[Category] = Seq(Pop, Science, Sports, Rock)

    def from(place: Int): Category = values(math.abs(place) % values.length)
  }

  case class Questions(var questions: Map[Category, Int]) {
    def pick(category: Category): String = {
      val i = questions(category)
      questions = questions + (category -> (i + 1))
      s"$category Question $i"
    }
  }

  object Questions {
    def apply(categories: Seq[Category]): Questions =
      new Questions(categories.map(c => c -> 0).toMap)
  }

  private object Messages {
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
      println(s"${player.name} now has ${player.purse.value} Gold Coins.")
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
