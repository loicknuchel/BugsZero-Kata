package com.adaptionsoft.games.uglytrivia

class Game(playerNames: Seq[String]) {

  import Game._

  assert(playerNames.size >= MinNumberOfPlayers, s"At least $MinNumberOfPlayers players")
  assert(playerNames.size <= MaxNumberOfPlayers, s"At max $MaxNumberOfPlayers players")

  private var questions = Category.values.map(c => c -> 0).toMap
  private val players = playerNames.map(name => Player(name, 0, Gold(0), inPenaltyBox = false))
  private var currentPlayerIndex = 0
  private var currentPlayer = players(currentPlayerIndex)

  Messages.start(players)

  def isPlayable: Boolean = howManyPlayers >= MinNumberOfPlayers

  def howManyPlayers: Int = players.size

  // return if the player has won
  def play(roll: Int, correctAnswer: Boolean): Boolean = {
    println(s"${currentPlayer.name} is the current player")
    println(s"They have rolled a $roll")
    var isGettingOutOfPenaltyBox = false
    if (currentPlayer.inPenaltyBox) {
      if (roll % 2 != 0) {
        isGettingOutOfPenaltyBox = true
        println(s"${currentPlayer.name} is getting out of the penalty box")
        movePlayerAndAskQuestion(currentPlayer, roll)
      } else {
        println(s"${currentPlayer.name} is not getting out of the penalty box")
        isGettingOutOfPenaltyBox = false
      }
    } else {
      movePlayerAndAskQuestion(currentPlayer, roll)
    }

    if (correctAnswer) {
      if (currentPlayer.inPenaltyBox) {
        if (isGettingOutOfPenaltyBox) {
          println("Answer was correct!!!!")
          nextPlayer()
          currentPlayer.purse += 1
          println(s"${currentPlayer.name} now has ${currentPlayer.purse.value} Gold Coins.")
          val winner = hasWon(currentPlayer)
          winner
        } else {
          nextPlayer()
          false
        }
      } else {
        println("Answer was corrent!!!!")
        currentPlayer.purse += 1
        println(s"${currentPlayer.name} now has ${currentPlayer.purse.value} Gold Coins.")
        val winner = hasWon(currentPlayer)
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
    currentPlayerIndex += 1
    if (currentPlayerIndex == players.size) currentPlayerIndex = 0
    currentPlayer = players(currentPlayerIndex)
  }

  private def movePlayerAndAskQuestion(player: Player, roll: Int): Unit = {
    player.place = (player.place + roll) % NumberOfCells
    println(s"${player.name}'s new location is ${player.place}")
    val category = Category.from(player.place)
    val i = questions(category)
    questions = questions + (category -> (i + 1))
    println(s"The category is $category")
    println(s"$category Question $i")
  }
}

object Game {

  val MinNumberOfPlayers = 2
  val MaxNumberOfPlayers = 6
  val NumberOfCells = 12

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

    val values: Seq[Category] = Seq(Pop, Science, Sports, Rock)

    def from(place: Int): Category = {
      place % 4 match {
        case 0 => Category.Pop
        case 1 => Category.Science
        case 2 => Category.Sports
        case _ => Category.Rock
      }
    }
  }

  private object Messages {
    def start(players: Seq[Player]): Unit = {
      players.zipWithIndex.foreach { case (p, i) =>
        println(s"${p.name} was added")
        println(s"They are player number ${i + 1}")
      }
    }
  }

  def hasWon(p: Player): Boolean = p.purse.value == 6
}
