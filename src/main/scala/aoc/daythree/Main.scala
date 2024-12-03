package aoc.daythree

import aoc.common.Utils

import scala.util.matching.Regex

object Main extends Utils:
  @main def partOne(args: String*): Unit =
    val rawCode = readResource("input.txt")

    val pattern = """mul\(\d+,\d+\)"""
    val regex = new Regex(pattern)
    val matches = regex.findAllMatchIn(rawCode).toSeq

    val products = matches.map{ m =>
      val numbers = new Regex("""\d+""").findAllMatchIn(m.toString())

      numbers.map(num => num.group(0).toInt).product
    }

    print(products.sum)

  @main def partTwo(args: String*): Unit =
    val rawCode = readResource("input.txt")

    val pattern = """mul\(\d+,\d+\)|do\(\)|don't\(\)"""
    val regex = new Regex(pattern)
    val matches = regex.findAllMatchIn(rawCode).toSeq

    val (products, enabled) = matches.foldLeft((List.empty[Int], true)){ case ((products, isEnabled), m) =>
      m.toString() match
        case "do()" => (products, true)
        case "don't()" => (products, false)
        case matched if isEnabled =>
          val numbers = new Regex("""\d+""").findAllMatchIn(matched)
          val product = numbers.map(num => num.group(0).toInt).product
          (product :: products, isEnabled)
        case _ => (products, isEnabled)
    }

    print(products.sum)


