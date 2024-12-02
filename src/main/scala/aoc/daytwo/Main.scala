package aoc.daytwo

import aoc.common.Utils

object Main extends Utils:
  @main def partOne(args: String*): Unit =
    val rawReports = loadResource("input.txt")

    val parsedReports = rawReports.map { r =>
      r.split(" ").map(_.toInt).toSeq
    }

    val reportSafeties = parsedReports.map { r =>
      val windows = r.sliding(2).toSeq
      val diffs = windows.map { w =>
        w.head - w.last
      }
      val signs = diffs.map(diff => diff.sign)
      val allIncreaseOrDecrease = signs.distinct.size == 1

      val unsafeDiffs = diffs.map(_.abs).filterNot(diff => diff >= 1 && diff <= 3)
      allIncreaseOrDecrease && unsafeDiffs.isEmpty
    }
    print(reportSafeties.count(_ == true))