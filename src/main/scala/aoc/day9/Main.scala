package aoc.day9

import aoc.common.Utils

import scala.annotation.tailrec
import scala.util.matching.Regex

object Main extends Utils:

  extension[T] (seq: Seq[T])
    def swapAt(fromIndex: Int, toIndex: Int): Seq[T] =
      if fromIndex == toIndex then seq
      else
        val (minIndex, maxIndex) = if fromIndex < toIndex then (fromIndex, toIndex) else (toIndex, fromIndex)
        val (beforeMin, rest) = seq.splitAt(minIndex)
        val (minElem, afterMin) = rest.splitAt(1)
        val (between, afterMax) = afterMin.splitAt(maxIndex - minIndex - 1)
        val (maxElem, after) = afterMax.splitAt(1)
        beforeMin ++ maxElem ++ between ++ minElem ++ after

  @tailrec
  private def moveToFreeSpace(blocks: Seq[String]): Seq[String] =
    val freeSpaceIndex = blocks.indexOf(".")
    val blockIndex = blocks.lastIndexWhere(_ != ".")
    if freeSpaceIndex == blockIndex + 1 then
      blocks
    else {
      val swappedBlocks = blocks.swapAt(freeSpaceIndex, blockIndex)
      moveToFreeSpace(swappedBlocks)
    }

  private def checksum(blocks: Seq[String]): Long =
    val indexedBlock = blocks.zipWithIndex
    val products = indexedBlock.map((block, index) =>
      if block == "." then 0
      else block.toLong * index.toLong)
    products.sum

  @main def partOne(args: String*): Unit =
    val rawDiskMap = readResource("input.txt")
    val filesAndFreeSpace = rawDiskMap.split("").map(_.toInt).sliding(2, 2).zipWithIndex.toSeq
    val rearrangedBlocks = filesAndFreeSpace.flatMap((fileAndFreeSpace, index) =>
        if fileAndFreeSpace.length == 1 then
          val numberFiles = fileAndFreeSpace.head
          val fileBlock = List.fill(numberFiles)(index.toString)
          fileBlock
        else
          val numberFiles = fileAndFreeSpace.head
          val freeSpace = fileAndFreeSpace.last
          val fileBlock = List.fill(numberFiles)(index.toString)
          val spaceBlock = List.fill(freeSpace)(".")
          fileBlock ++ spaceBlock
    )
    val shuffled = moveToFreeSpace(rearrangedBlocks)
    println(shuffled)
    println(checksum(shuffled))

