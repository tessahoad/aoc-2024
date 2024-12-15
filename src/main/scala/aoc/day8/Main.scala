package aoc.day8

import aoc.common.Utils

import scala.annotation.tailrec
import scala.util.matching.Regex

object Main extends Utils:

  def externalSection(x1: Float, y1: Float, x2: Float, y2: Float, ratio: Float): (Float, Float) =
    val x = (ratio * x2 - x1) / (ratio - 1)
    val y = (ratio * y2 - y1) / (ratio - 1)
    (x, y)

  @main def partOne(args: String*): Unit =
    val antennaMap = readResourceLines("input.txt")
    val coordinates = antennaMap.zipWithIndex.flatMap((mapRow, rowIndex) =>
      mapRow.zipWithIndex.map((mapEntry, columnIndex) =>
        (mapEntry, rowIndex.toFloat, columnIndex.toFloat)
      ))
    val antennaGroups = coordinates.groupBy(_._1).filterNot((mapEntry, _) => mapEntry == '.')
    val antennaCombinations = antennaGroups.map((mapEntry, coordinates) =>
      (mapEntry, coordinates.combinations(2).map {
        case Seq(a, b) => ((a._2, a._3), (b._2, b._3))
      }.toSeq)
    )
    val antennaAntinodes = antennaCombinations.map((mapEntry, combinations) =>
      (mapEntry, combinations.flatMap { case ((x1, y1), (x2, y2)) =>
//      https://brilliant.org/wiki/section-formula/#external-divisions-with-section-formula
        val firstAntinode = externalSection(x1, y1, x2, y2, 2)
        val secondAntinode = externalSection(x2, y2, x1, y1, 2)
        Seq(firstAntinode, secondAntinode)
      })
    )
    val antinodesOnMap = antennaAntinodes.map((mapEntry, antinodes) =>
      (mapEntry, antinodes.filter(antinode =>
        coordinates.exists((mapEntry, rowIndex, columnIndex) =>
          (rowIndex, columnIndex) == antinode

      ))
    ))
    val allAntinodes = antinodesOnMap.values.flatten.toSeq.distinct.size
    println(allAntinodes)

