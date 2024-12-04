package aoc.day4

import aoc.common.Utils

import scala.util.matching.Regex

object Main extends Utils:

  def twoDimensionalCharArrayToListString(twoDimensionalCharArray: List[(String, Int, Int)], transformer: ((String, Int, Int)) => Any): Iterable[String] = {
    twoDimensionalCharArray
      .groupBy(transformer)
      .map((index, listOfChars) => listOfChars.map((char, rowIndex, columnIndex) => char).mkString)
  }

  def countXMASMatches(strings: Iterable[String]): Int = {
    strings.map(row => "XMAS".r.findAllMatchIn(row).count(_ => true) + "XMAS".r.findAllMatchIn(row.reverse).count(_ => true)).sum
  }

  @main def partOne(args: String*): Unit =
    val rawWordSearch = readResourceLines("input.txt")

    val searchSplitIntoChars = rawWordSearch.map(line => line.split("").toSeq)
    val transposed = searchSplitIntoChars.transpose

    val charsWithRowAndColumn = searchSplitIntoChars.zipWithIndex.flatMap((listOfChars, rowIndex) =>
      listOfChars.zipWithIndex.map((char, columnIndex) =>
        (char, rowIndex, columnIndex)
      ))

    val rows = twoDimensionalCharArrayToListString(charsWithRowAndColumn, (char, rowIndex, columnIndex) => rowIndex)
    val columns = twoDimensionalCharArrayToListString(charsWithRowAndColumn, (char, rowIndex, columnIndex) => columnIndex)
    val leftToRightDiagonals = twoDimensionalCharArrayToListString(charsWithRowAndColumn, (char, rowIndex, columnIndex) => rowIndex + columnIndex)
    val rightToLeftDiagonals = twoDimensionalCharArrayToListString(charsWithRowAndColumn, (char, rowIndex, columnIndex) => rowIndex - columnIndex)


    val rowMatches = countXMASMatches(rows)
    val columnMatches = countXMASMatches(columns)
    val leftToRightDiagonalsMatches = countXMASMatches(leftToRightDiagonals)
    val rightToLeftDiagonalsMatches = countXMASMatches(rightToLeftDiagonals)
    print(rowMatches + columnMatches + leftToRightDiagonalsMatches + rightToLeftDiagonalsMatches)

  @main def partTwo(args: String*): Unit =
    val rawWordSearch = readResourceLines("input.txt")

    val searchSplitIntoChars = rawWordSearch.map(line => line.split("").toSeq)
    val transposed = searchSplitIntoChars.transpose

    val charsWithRowAndColumn = searchSplitIntoChars.zipWithIndex.flatMap((listOfChars, rowIndex) =>
      listOfChars.zipWithIndex.map((char, columnIndex) =>
        (char, rowIndex, columnIndex)
      ))

    val numRows = transposed.head.length
    val numCols = searchSplitIntoChars.head.length

    val threeByThreeBlocks = for {
      row <- 0 until numRows - 2
      col <- 0 until numCols - 2
    } yield {
      Seq(
        Seq(searchSplitIntoChars(row)(col), searchSplitIntoChars(row)(col + 1), searchSplitIntoChars(row)(col + 2)),
        Seq(searchSplitIntoChars(row + 1)(col), searchSplitIntoChars(row + 1)(col + 1), searchSplitIntoChars(row + 1)(col + 2)),
        Seq(searchSplitIntoChars(row + 2)(col), searchSplitIntoChars(row + 2)(col + 1), searchSplitIntoChars(row + 2)(col + 2))
      )
    }

    val blocksWithXMAS = threeByThreeBlocks.map { block => {
      val centralCharacter = block(1)(1)
      val topLeftCornerCharacter = block(0)(0)
      val topRightCornerCharacter = block(0)(2)
      val bottomLeftCornerCharacter = block(2)(0)
      val bottomRightCornerCharacter = block(2)(2)
      val validLeftToRightDiagonal = centralCharacter == "A" && ((topLeftCornerCharacter == "M" && bottomRightCornerCharacter == "S") || (topLeftCornerCharacter == "S" && bottomRightCornerCharacter == "M"))
      val validRightToLeftDiagonal = centralCharacter == "A" && ((topRightCornerCharacter == "M" && bottomLeftCornerCharacter == "S") || (topRightCornerCharacter == "S" && bottomLeftCornerCharacter == "M"))
      validRightToLeftDiagonal && validLeftToRightDiagonal
    }}

    println(blocksWithXMAS.count(_ == true))







