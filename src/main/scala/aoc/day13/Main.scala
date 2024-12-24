package aoc.day13

import aoc.common.Utils
import aoc.day8.Main.readResourceLines

import scala.collection.mutable
import breeze.linalg._

object Main extends Utils:

  def solveSimultaneousEquations(a1: Double, b1: Double, c1: Double, a2: Double, b2: Double, c2: Double): (Double, Double) = {
    val A = DenseMatrix((a1, b1), (a2, b2))
    val C = DenseVector(c1, c2)
    val X = inv(A) * C
    val scale = math.pow(10, 6)
    (math.round(X(0) * scale) / scale, math.round(X(1) * scale) / scale)
  }

  case class Button(xTransformer: Double, yTransformer: Double)
  case class Prize(xValue: Double, yValue: Double)

  def main(args: Array[String]): Unit = {
    val rawConfigurations = readResourceLines("input.txt")
    val configs = rawConfigurations
      .filterNot(_.isEmpty)
      .sliding(3, 3)
      .map(config =>
        val aInstructions: Array[String] = config.head.split(":").last.split(",")
        val buttonA = Button(
          aInstructions.head.split("\\+").last.toDouble,
          aInstructions.last.split("\\+").last.toDouble
        )
        val bInstructions = config(1).split(":").last.split(",")
        val buttonB = Button(
          bInstructions.head.split("\\+").last.toDouble,
          bInstructions.last.split("\\+").last.toDouble
        )

        val prizeInstructions = config.last.split(":").last.split(",")
        val prize = Prize(
          prizeInstructions.head.split("=").last.toDouble,
          prizeInstructions.last.split("=").last.toDouble
        )

        (buttonA, buttonB, prize)
      )
      .toSeq
    val answers = configs.map(config =>
      solveSimultaneousEquations(config._1.xTransformer, config._2.xTransformer, config._3.xValue, config._1.yTransformer, config._2.yTransformer, config._3.yValue)
    ).filter(answer => answer._1.isWhole && answer._2.isWhole)

    val tokens = answers.map(answer => 3*answer._1 + answer._2)

    println(tokens.sum)
  }

