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


