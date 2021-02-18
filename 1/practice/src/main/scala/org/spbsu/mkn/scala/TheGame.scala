package org.spbsu.mkn.scala

import scala.io.StdIn.readLine
import scala.util.Random
import scala.util.control.Breaks._


object TheGame {

  def generateNumberString(length: Int, chars: Set[Char] = (('A' to 'Z') ++ ('0' to '9')).toSet): String = {
    val char = chars.iterator.drop(Random.nextInt(chars.size)).next()
    length match {
      case 0 => ""
      case _ => char.toString + generateNumberString(length - 1, chars.diff(Set(char)))
    }
  }

  def validate(secret: String, userInput: String, numTries: Int = 1): GuessResult = {
    if (secret.length != userInput.length) throw new WrongNumberLengthException(secret.length, userInput.length)

    if (secret == userInput) {
      if (userInput.toSet.size != userInput.length) throw new RepeatingDigitsException
      return Correct(numTries)
    }

    val bulls = secret.zip(userInput).map { case (a, b) => a == b }.count(x => x)
    val cows = secret.toSet.intersect(userInput.toSet).size
    Incorrect(bulls, cows - bulls)
  }

  def main(args: Array[String]): Unit = {
    print("Enter your name: ")
    val name = readLine()
    println(s"Hello, $name!")

    println("Enter secret's length: ")
    val length = readLine().toInt

    val secret = generateNumberString(length)
    var numTries = 0

    breakable {
      while (true) {
        numTries += 1
        println("Enter your guess: ")
        val guess = readLine()
        try {
          validate(secret, guess, numTries) match {
            case Correct(x) =>
              println(s"Correct, $x tries total")
              break
            case Incorrect(bulls, cows) => println(s"Bulls: $bulls, cows: $cows")
          }

        } catch {
          case _: WrongNumberLengthException => println("Wrong number length")
          case _: RepeatingDigitsException => println("Repeating digits")
        }
      }
    }
  }

  sealed trait GuessResult

  case class Correct(numTries: Int) extends GuessResult

  case class Incorrect(bulls: Int, cows: Int) extends GuessResult

  class RepeatingDigitsException extends RuntimeException

  class WrongNumberLengthException(expected: Int, got: Int) extends RuntimeException
}
