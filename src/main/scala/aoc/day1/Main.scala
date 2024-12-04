package aoc.day1

import scala.io.Source

@main def partOne(args: String*): Unit =
  val source = Source.fromResource("aoc/day1/input.txt")
  val lines = source.getLines().toList
  source.close()

  val stringses = lines.map(line => line.split(" {3}"))
  val firstList = stringses.map(_(0)).map(_.toInt).sorted(Ordering[Int])
  val secondList = stringses.map(_(1)).map(_.toInt).sorted(Ordering[Int])

  val sizeDiff = firstList.zip(secondList).map((a, b) => (a - b).abs).sum

  println(sizeDiff)

@main def partTwo(args: String*): Unit =
  val source = Source.fromResource("aoc/day1/input.txt")
  val lines = source.getLines().toList
  source.close()

  val stringses = lines.map(line => line.split(" {3}"))
  val firstList = stringses.map(_(0)).map(_.toInt)
  val secondList = stringses.map(_(1)).map(_.toInt)

  val freqMap = secondList.foldLeft(Map.empty[Int, Int]) { (acc, value) =>
    val runningTotal = acc.getOrElse(value, 0)
    acc + (value -> (runningTotal + 1))
  }

  val simScores = firstList.map{ value =>
    freqMap.getOrElse(value, 0) * value
  }

  println(simScores.sum)