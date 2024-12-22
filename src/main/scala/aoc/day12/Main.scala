package aoc.day12

import aoc.common.Utils
import aoc.day8.Main.readResourceLines

import scala.collection.mutable

object Main extends Utils:

  case class Plot(plant: String, coord: (Int, Int))

  def getNeighbours(plot: Plot, plots: Set[Plot]): Set[Plot] =
    val (rowIndex, columnIndex) = plot.coord
    plots.filter((otherPlot) =>
      val (otherRowIndex, otherColumnIndex) = otherPlot.coord
      (otherRowIndex == rowIndex - 1 && otherColumnIndex == columnIndex) ||
      (otherRowIndex == rowIndex + 1 && otherColumnIndex == columnIndex) ||
      (otherRowIndex == rowIndex && otherColumnIndex == columnIndex - 1) ||
      (otherRowIndex == rowIndex && otherColumnIndex == columnIndex + 1)
    )

  def breadthFirstSearch(root: Plot, nodes: Set[Plot]): Set[Plot] =
    val visited = mutable.Set[Plot]()
    val queue = mutable.Queue[Plot]()
    queue.enqueue(root)
    while queue.nonEmpty do
      val current = queue.dequeue()
      if root.plant == current.plant && !visited.contains(current) then
        visited.add(current)
        val neighbours = getNeighbours(current, nodes)
        neighbours.foreach(queue.enqueue)
    visited.toSet

  @main def partOne(args: String*): Unit =
    val farmMap = readResourceLines("input.txt")
    val plots = farmMap.zipWithIndex.flatMap((mapRow, rowIndex) =>
      mapRow.zipWithIndex.map((mapEntry, columnIndex) =>
        Plot(mapEntry.toString, (rowIndex, columnIndex))
      ))

    val regions = mutable.Set[Set[Plot]]()
    val visited = mutable.Set[Plot]()

    plots.foreach(plot =>
      if !visited.contains(plot) then
        val region = breadthFirstSearch(plot, plots.toSet)
        regions.add(region)
        visited.addAll(region)
    )

    val products = regions.toSeq.map(region =>
      val area = region.size
      val perimeters = region.toSeq.map(plot =>
        4 - getNeighbours(plot, region).size
      )
      area * perimeters.sum
    )

    println(products.sum)

