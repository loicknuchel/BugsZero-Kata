package com.adaptionsoft.games.trivia.runner

import java.util.Random

import com.adaptionsoft.games.uglytrivia.Game

object GameRunner {
  private var isAWinner = true

  def main(args: Array[String]): Unit = {
    val rand = new Random
    playGame(rand)
  }

  def playGame(rand: Random): Unit = {

    val aGame = new Game(Seq("Chet", "Pat", "Sue"))
    do {
      aGame.roll(rand.nextInt(5) + 1)
      isAWinner = aGame.hasAnswered(rand.nextInt(9) != 7)
    } while (!isAWinner)
  }
}
