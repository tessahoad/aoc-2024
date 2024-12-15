package aoc.day10

import aoc.common.Utils
import aoc.day8.Main.readResourceLines

import scala.collection.mutable

object Main extends Utils:

  case class TreeNode[T](value: T, children: Seq[TreeNode[T]])

  def buildTree[T](nodes: mutable.Map[T, Seq[T]], rootValue: T): TreeNode[T] =
    def buildNode(value: T): TreeNode[T] =
      val children = nodes.getOrElse(value, Seq.empty).map(buildNode)
      TreeNode(value, children)
    buildNode(rootValue)


  def findUniqueLeafs[T](node: TreeNode[T]): Set[T] = {
    if (node.children.isEmpty) {
      Set(node.value)
    } else {
      node.children.flatMap(findUniqueLeafs).toSet
    }
  }

  def memoiseTrails(coordinates: Seq[(Int, (Int, Int))]): mutable.Map[(Int, (Int, Int)), Seq[(Int, (Int, Int))]]  =
    val trailMap: mutable.Map[(Int, (Int, Int)), Seq[(Int, (Int, Int))]] = mutable.Map()
    coordinates.foreach { case (elevation, coord) =>
      val (rowIndex, columnIndex) = coord
      val possibleSteps = Seq(
        (rowIndex - 1, columnIndex),
        (rowIndex + 1, columnIndex),
        (rowIndex, columnIndex - 1),
        (rowIndex, columnIndex + 1),
      )
      val validSteps = coordinates.filter { case (mapElevation, (rowIndex, columnIndex)) =>
        possibleSteps.contains((rowIndex, columnIndex)) && mapElevation == elevation + 1
      }
      trailMap((elevation, coord)) = validSteps
    }
    trailMap

  @main def partOne(args: String*): Unit =
    val islandMap = readResourceLines("input.txt")
    val coordinates = islandMap.zipWithIndex.flatMap((mapRow, rowIndex) =>
      mapRow.zipWithIndex.map((mapEntry, columnIndex) =>
        (mapEntry.toString.toInt, (rowIndex, columnIndex))
      ))

    val trailMap = memoiseTrails(coordinates)
    val trailHeads = trailMap.filter((key, value) => key._1 == 0)
    val trees = trailHeads.map(head => buildTree(trailMap, head._1))
    val leaves = trees.map(findUniqueLeafs).map(uniqueLeaves => uniqueLeaves.filter(_._1 == 9))

    println(leaves.map(_.size).sum)

