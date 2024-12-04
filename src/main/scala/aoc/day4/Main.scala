package aoc.day4

import aoc.common.Utils

import scala.util.matching.Regex

object Main extends Utils:
  
  def twoDimensionalCharArrayToListString(twoDimensionalCharArray: List[(String, Int, Int)], transformer: ((String, Int, Int)) => Any) : Iterable[String] = {
    twoDimensionalCharArray
      .groupBy(transformer)
      .map((index, listOfChars) => listOfChars.map((char, rowIndex, columnIndex) => char).mkString)
  }
  
  def countXMASMatches(strings: Iterable[String]) : Int = {
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



