package com.adaptionsoft.games.trivia.runner

import java.util.Random

import com.adaptionsoft.games.uglytrivia.Game

object GameRunner {
  def main(args: Array[String]): Unit = {
    val rand = new Random
    playGame(rand)
  }

  def playGame(rand: Random): Unit = {
    val aGame = new Game(Seq("Chet", "Pat", "Sue"))
    var isAWinner = false
    while (!isAWinner) {
      val roll = rand.nextInt(5) + 1
      val correctAnswer = rand.nextInt(9) != 7
      isAWinner = aGame.play(roll, correctAnswer)
    }
  }
}
