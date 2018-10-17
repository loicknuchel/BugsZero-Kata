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
    var hasWon = false
    while (!hasWon) {
      val roll = rand.nextInt(5) + 1
      val answerCorrectly = rand.nextInt(9) != 7
      hasWon = aGame.play(roll, answerCorrectly)
    }
  }
}
