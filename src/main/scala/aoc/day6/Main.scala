package aoc.day6

import aoc.common.Utils

import scala.annotation.tailrec
import scala.util.matching.Regex

object Main extends Utils:

  enum Direction:
    def rotateClockwise: Direction = this match
      case NORTH => EAST
      case EAST => SOUTH
      case SOUTH => WEST
      case WEST => NORTH

    case NORTH
    case EAST
    case SOUTH
    case WEST

  object Direction:
    def fromChar(value: Char): Option[Direction] = value match
      case '^' => Some(Direction.NORTH)
      case 'v' => Some(Direction.SOUTH)
      case '<' => Some(Direction.WEST)
      case '>' => Some(Direction.EAST)
      case _ => None


  enum MapEntry(val allowedValues: Seq[Char]):
    case BLANK    extends MapEntry(Seq('.'))
    case GUARD    extends MapEntry(Seq('^', 'v', '<', '>'))
    case OBSTACLE extends MapEntry(Seq('#'))

  object MapEntry:
    def fromChar(value: Char): MapEntry =
      values.find(_.allowedValues.contains(value)).getOrElse(BLANK)

  case class MapState(map: List[(MapEntry, Int, Int)], guardPosition: (Int, Int), direction: Direction, visited: Seq[(Int, Int)])

  @tailrec
  def guardWalk(mapState: MapState): MapState = {
    val nextStep = mapState.direction match
      case Direction.NORTH => (mapState.guardPosition._1 - 1, mapState.guardPosition._2)
      case Direction.EAST => (mapState.guardPosition._1, mapState.guardPosition._2 + 1)
      case Direction.SOUTH => (mapState.guardPosition._1 + 1, mapState.guardPosition._2)
      case Direction.WEST => (mapState.guardPosition._1, mapState.guardPosition._2 - 1)
    mapState.map.find((mapEntry, rowIndex, columnIndex) => rowIndex == nextStep._1 && columnIndex == nextStep._2) match
      case Some((MapEntry.OBSTACLE, _, _)) => guardWalk(mapState.copy(direction = mapState.direction.rotateClockwise))
      case Some((_, _, _)) => guardWalk(MapState(mapState.map, nextStep, mapState.direction, mapState.visited :+ nextStep))
      case None => mapState
  }


  @main def partOne(args: String*): Unit =
    val rawMap = readResourceLines("input.txt")
    val initialState: MapState = getInitialState(rawMap)
    val finalState = guardWalk(initialState)
    print(finalState.visited.distinct.size)

  private def getInitialState(rawMap: List[String]) = {
    val coordinates = rawMap.zipWithIndex.flatMap((mapRow, rowIndex) =>
      mapRow.zipWithIndex.map((mapEntry, columnIndex) =>
        (mapEntry, rowIndex, columnIndex)
      ))
    val maybeGuardCoordinates = coordinates
      .find((mapEntry, rowIndex, columnIndex) => MapEntry.fromChar(mapEntry) == MapEntry.GUARD)
    val guardInitialPosition = maybeGuardCoordinates
      .map((mapEntry, rowIndex, columnIndex) => (rowIndex, columnIndex)).get
    val guardInitialDirection = maybeGuardCoordinates
      .map((mapEntry, rowIndex, columnIndex) => Direction.fromChar(mapEntry).get).get

    val startingMap = coordinates.map((mapEntry, rowIndex, columnIndex) => (MapEntry.fromChar(mapEntry), rowIndex, columnIndex))

    val initialState = MapState(startingMap, guardInitialPosition, guardInitialDirection, Seq(guardInitialPosition))
    initialState
  }







