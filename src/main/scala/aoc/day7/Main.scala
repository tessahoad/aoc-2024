package aoc.day7

import aoc.common.Utils

import scala.annotation.tailrec
import scala.util.matching.Regex

object Main extends Utils:

  case class Equation(solution: Long, operands: Seq[Long])

  def reduceOperands(operands: Seq[Long]): Seq[Long] = {
    operands.tail.foldLeft(Seq(operands.head)) { case (acc, a) =>
       acc.flatMap(value => Seq(value + a, value * a))
    }.distinct
  }

  def reduceOperands2(operands: Seq[Long]): Seq[Long] = {
    operands.tail.foldLeft(Seq(operands.head)) { case (acc, a) =>
      acc.flatMap{value =>
        val concat = value.toString + a.toString
        Seq(value + a, value * a, concat.toLong)
      }
    }.distinct
  }

  @main def partOne(args: String*): Unit =
    val rawEquations = readResourceLines("input.txt")
    val parsedEquations = rawEquations
      .map(raw => raw.split(":"))
      .map(equation => (equation.head, equation.last))
      .map((solutionString, operandsString) => Equation(solutionString.toLong, operandsString.trim.split(" ").map(_.toLong)))

    val validEquations = parsedEquations.filter(equation => reduceOperands(equation.operands).contains(equation.solution))
    print(validEquations.map(_.solution).sum)

  @main def partTwo(args: String*): Unit =
    val rawEquations = readResourceLines("input.txt")
    val parsedEquations = rawEquations
      .map(raw => raw.split(":"))
      .map(equation => (equation.head, equation.last))
      .map((solutionString, operandsString) => Equation(solutionString.toLong, operandsString.trim.split(" ").map(_.toLong)))

    val validEquations = parsedEquations.filter(equation => reduceOperands2(equation.operands).contains(equation.solution))
    print(validEquations.map(_.solution).sum)
