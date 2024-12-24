package aoc.day13

import aoc.common.Utils
import aoc.day8.Main.readResourceLines

object Main extends Utils:

  def numberTokens(a1: Long, b1: Long, c1: Long, a2: Long, b2: Long, c2: Long): Long = {
    val a = (c1 * b2 - c2 * b1) / (a1 * b2 - a2 * b1)
    val b = (c1 - a1 * a) / b1
    if (a * a1 + b * b1 == c1 && a * a2 + b * b2 == c2) 3 * a + b else 0
  }

  case class Button(xTransformer: Long, yTransformer: Long)
  case class Prize(xValue: Long, yValue: Long)

  @main def partOne(args: String*): Unit = {
    val rawConfigurations = readResourceLines("input.txt")
    val configs = rawConfigurations
      .filterNot(_.isEmpty)
      .sliding(3, 3)
      .map(config =>
        val aInstructions: Array[String] = config.head.split(":").last.split(",")
        val buttonA = Button(
          aInstructions.head.split("\\+").last.toLong,
          aInstructions.last.split("\\+").last.toLong
        )
        val bInstructions = config(1).split(":").last.split(",")
        val buttonB = Button(
          bInstructions.head.split("\\+").last.toLong,
          bInstructions.last.split("\\+").last.toLong
        )

        val prizeInstructions = config.last.split(":").last.split(",")
        val prize = Prize(
          prizeInstructions.head.split("=").last.toLong,
          prizeInstructions.last.split("=").last.toLong
        )

        (buttonA, buttonB, prize)
      )
      .toSeq
    val tokens = configs.map(config =>
      numberTokens(config._1.xTransformer, config._2.xTransformer, config._3.xValue, config._1.yTransformer, config._2.yTransformer, config._3.yValue)
    )

    println(tokens.sum)
  }

  @main def partTwo(args: String*): Unit =
    val rawConfigurations = readResourceLines("input.txt")
    val configs = rawConfigurations
      .filterNot(_.isEmpty)
      .sliding(3, 3)
      .map(config =>
        val aInstructions: Array[String] = config.head.split(":").last.split(",")
        val buttonA = Button(
          aInstructions.head.split("\\+").last.toLong,
          aInstructions.last.split("\\+").last.toLong
        )
        val bInstructions = config(1).split(":").last.split(",")
        val buttonB = Button(
          bInstructions.head.split("\\+").last.toLong,
          bInstructions.last.split("\\+").last.toLong
        )

        val prizeInstructions = config.last.split(":").last.split(",")
        val prize = Prize(
          prizeInstructions.head.split("=").last.toLong + 10000000000000L,
          prizeInstructions.last.split("=").last.toLong + 10000000000000L
        )

        (buttonA, buttonB, prize)
      )
      .toSeq
    val tokens = configs.map(config =>
      numberTokens(config._1.xTransformer, config._2.xTransformer, config._3.xValue, config._1.yTransformer, config._2.yTransformer, config._3.yValue)
    )

    println(tokens.sum)

