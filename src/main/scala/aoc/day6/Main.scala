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

  case class MapState(map: List[(MapEntry, Int, Int)], guardPosition: (Int, Int), direction: Direction, visited: Seq[(Int, Int, Direction)], isInfiniteLoop: Boolean) {
    def hasVisitedBefore: Boolean = visited.groupBy(identity).view.mapValues(_.size).values.exists(_ > 1)
  }

  @tailrec
  def guardWalk(mapState: MapState): MapState = {
    if mapState.hasVisitedBefore then return mapState.copy(isInfiniteLoop = true)
    val nextStep = mapState.direction match
      case Direction.NORTH => (mapState.guardPosition._1 - 1, mapState.guardPosition._2)
      case Direction.EAST => (mapState.guardPosition._1, mapState.guardPosition._2 + 1)
      case Direction.SOUTH => (mapState.guardPosition._1 + 1, mapState.guardPosition._2)
      case Direction.WEST => (mapState.guardPosition._1, mapState.guardPosition._2 - 1)
    mapState.map.find((mapEntry, rowIndex, columnIndex) => rowIndex == nextStep._1 && columnIndex == nextStep._2) match
      case Some((MapEntry.OBSTACLE, _, _)) => guardWalk(mapState.copy(direction = mapState.direction.rotateClockwise, visited = mapState.visited :+ (mapState.guardPosition._1, mapState.guardPosition._2, mapState.direction.rotateClockwise), isInfiniteLoop = false))
      case Some((_, _, _)) => guardWalk(MapState(mapState.map, nextStep, mapState.direction, mapState.visited :+ (nextStep._1, nextStep._2, mapState.direction), false))
      case None => mapState
  }


  @main def partOne(args: String*): Unit =
    val rawMap = readResourceLines("input.txt")
    val initialState: MapState = getInitialState(rawMap)
    val finalState = guardWalk(initialState)
    print(finalState.visited.map(location =>  (location._1, location._2)).distinct.size)

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

    val initialState = MapState(startingMap, guardInitialPosition, guardInitialDirection, Seq((guardInitialPosition._1, guardInitialPosition._2, guardInitialDirection)), false)
    initialState
  }

  @main def partTwo(args: String*): Unit =

    def updateMap(map: List[(MapEntry, Int, Int)], oldEntry: (MapEntry, Int, Int), newEntry: (MapEntry, Int, Int)): List[(MapEntry, Int, Int)] = {
      map.indexOf(oldEntry) match
        case -1 => map
        case index => map.updated(index, newEntry)
    }

    val rawMap = readResourceLines("input.txt")
    val initialState = getInitialState(rawMap)

    val finalState = guardWalk(initialState)
    val mapStatesToCheckForLoops = finalState.visited.distinct.map { case (rowIndex, columnIndex, direction) =>
      val guardStartingLocation = (rowIndex, columnIndex)
      val guardDirection = direction
      val newObstacleLocation = guardDirection match
        case Direction.NORTH => (rowIndex - 1, columnIndex)
        case Direction.EAST => (rowIndex, columnIndex + 1)
        case Direction.SOUTH => (rowIndex + 1, columnIndex)
        case Direction.WEST => (rowIndex, columnIndex - 1)

      val obstacleUpdatedMap = updateMap(initialState.map, (MapEntry.BLANK, newObstacleLocation._1, newObstacleLocation._2), (MapEntry.OBSTACLE, newObstacleLocation._1, newObstacleLocation._2))
      val initialGuardPositionRemovedMap = updateMap(obstacleUpdatedMap, (MapEntry.GUARD, initialState.guardPosition._1, initialState.guardPosition._2), (MapEntry.BLANK, initialState.guardPosition._1, initialState.guardPosition._2))
      val newGuardPositionMap = updateMap(initialGuardPositionRemovedMap, (MapEntry.BLANK, guardStartingLocation._1, guardStartingLocation._2), (MapEntry.GUARD, guardStartingLocation._1, guardStartingLocation._2))
      MapState(newGuardPositionMap, guardStartingLocation, guardDirection, Seq((guardStartingLocation._1, guardStartingLocation._2, guardDirection)), false)
    }

    val finalStates = mapStatesToCheckForLoops.zipWithIndex.map((mapState, index) => {
      if index % 10 == 0 then println(s"Checking map state $index")
      guardWalk(mapState)
    })

    println(finalStates.count(_.isInfiniteLoop))







