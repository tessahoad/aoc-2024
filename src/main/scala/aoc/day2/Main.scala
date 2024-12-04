package aoc.day2

import aoc.common.Utils

object Main extends Utils:
  @main def partOne(args: String*): Unit =
    val rawReports = readResourceLines("input.txt")

    val parsedReports = rawReports.map { r =>
      r.split(" ").map(_.toInt).toSeq
    }

    val reportSafeties = parsedReports.map { r =>
      isSafe(r)
    }
    print(reportSafeties.count(_ == true))

  private def isSafe(report: Seq[Int]) = {
    val windows = report.sliding(2).toSeq
    val diffs = windows.map { w =>
      w.head - w.last
    }
    val signs = diffs.map(diff => diff.sign)
    val allIncreaseOrDecrease = signs.distinct.size == 1

    val unsafeDiffs = diffs.map(_.abs).filterNot(diff => diff >= 1 && diff <= 3)
    allIncreaseOrDecrease && unsafeDiffs.isEmpty
  }

  @main def partTwo(args: String*): Unit =
    val rawReports = readResourceLines("input.txt")

    val parsedReports = rawReports.map { r =>
      r.split(" ").map(_.toInt).toSeq
    }

    val reportSafeties = parsedReports.map { report =>
      if isSafe(report) then
        true
      else
        val dampenedReports = report.indices.map { i =>
          report.patch(i, Nil, 1)
        }
        dampenedReports.map(isSafe).count(_ == true) > 0
    }

    println(reportSafeties.count(_ == true))


