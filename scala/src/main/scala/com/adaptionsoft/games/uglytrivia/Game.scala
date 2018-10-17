package com.adaptionsoft.games.uglytrivia

class Game(player1: String, player2: String, otherPlayers: String*) {

  import Game._

  // TODO: create a GameState case class
  private var questions = Category.values.map(c => c -> 0).toMap
  private val players = (player1 +: player2 +: otherPlayers).map(name => Player(name, 0, Gold(0), inPenaltyBox = false))
  private var currentPlayerIndex = 0
  private var currentPlayer = players(currentPlayerIndex)

  Messages.start(players)

  def isPlayable: Boolean = true

  def howManyPlayers: Int = players.size

  // TODO: simplify business logic
  // return if the player has won
  def play(roll: Int, correctAnswer: Boolean): Boolean = {
    println(s"${currentPlayer.name} is the current player")
    println(s"They have rolled a $roll")
    if (currentPlayer.inPenaltyBox) {
      if (canExitPenaltyBox(roll)) {
        println(s"${currentPlayer.name} is getting out of the penalty box")
        movePlayerAndAskQuestion(currentPlayer, roll)
      } else {
        println(s"${currentPlayer.name} is not getting out of the penalty box")
      }
    } else {
      movePlayerAndAskQuestion(currentPlayer, roll)
    }

    if (correctAnswer) {
      if (currentPlayer.inPenaltyBox) {
        if (canExitPenaltyBox(roll)) {
          println("Answer was correct!!!!")
          // FIXME: should exit penalty box
          nextPlayer() // FIXME: should be just before returning, will fix wrong gold attribution
          currentPlayer.addGold(1)
          println(s"${currentPlayer.name} now has ${currentPlayer.purse.value} Gold Coins.")
          val winner = currentPlayer.hasWon
          winner
        } else {
          nextPlayer()
          false
        }
      } else {
        println("Answer was corrent!!!!") // FIXME: typo
        currentPlayer.addGold(1)
        println(s"${currentPlayer.name} now has ${currentPlayer.purse.value} Gold Coins.")
        val winner = currentPlayer.hasWon
        nextPlayer()
        winner
      }
    } else {
      println("Question was incorrectly answered")
      println(s"${currentPlayer.name} was sent to the penalty box")
      currentPlayer.inPenaltyBox = true
      nextPlayer()
      false
    }
  }

  private def nextPlayer(): Unit = {
    currentPlayerIndex = (currentPlayerIndex + 1) % players.size
    currentPlayer = players(currentPlayerIndex)
  }

  private def canExitPenaltyBox(roll: Int): Boolean = roll % 2 == 1

  // TODO: do not update param player
  // TODO: do not update Game state
  private def movePlayerAndAskQuestion(player: Player, roll: Int): Unit = {
    player.move(roll)
    println(s"${player.name}'s new location is ${player.place}")
    val category = Category.from(player.place)
    val i = questions(category)
    questions = questions + (category -> (i + 1))
    println(s"The category is $category")
    println(s"$category Question $i")
  }
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

  private object Messages {
    def start(players: Seq[Player]): Unit = {
      players.zipWithIndex.foreach { case (p, i) =>
        println(s"${p.name} was added")
        println(s"They are player number ${i + 1}")
      }
    }
  }

}
