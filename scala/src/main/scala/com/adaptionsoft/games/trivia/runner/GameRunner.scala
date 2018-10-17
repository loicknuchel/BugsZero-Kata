package com.adaptionsoft.games.trivia.runner

import java.util.Random

import com.adaptionsoft.games.uglytrivia.Game

object GameRunner {
  def main(args: Array[String]): Unit = {
    val rand = new Random
    playGame(rand)
  }

  def playGame(rand: Random): Unit = {
    val aGame = new Game("Chet", "Pat", "Sue")
    var notAWinner = true
    while (notAWinner) {
      val roll = rand.nextInt(5) + 1
      val answeredCorrectly = rand.nextInt(9) != 7
      notAWinner = aGame.play(roll, answeredCorrectly)
    }
  }
}
