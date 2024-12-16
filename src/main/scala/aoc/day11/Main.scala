package aoc.day11

import aoc.common.Utils
import aoc.day8.Main.readResourceLines

import scala.collection.mutable

object Main extends Utils:

  def dropLeadingZeroes(stone: String): String =
    val stripped = stone.dropWhile(_ == '0')
    if stripped.isEmpty then "0" else stripped

  def blink(stones: Seq[String]): Seq[String] =
    stones.flatMap {
      case "0" => Seq("1")
      case stone if stone.length % 2 == 0 =>
        val (leftStone, rightStone) = stone.splitAt(stone.length /2)
        Seq(dropLeadingZeroes(leftStone), dropLeadingZeroes(rightStone))
      case stone => Seq((stone.toLong * 2024).toString)
    }

  @main def partOne(args: String*): Unit =
    val unparsedStones = readResource("input.txt")
    val stones = unparsedStones.split(" ").toSeq
    println("Initial arrangement: " + stones.mkString(" "))
    val manyBlinks = (1 to 25).foldLeft(stones)((acc, index) =>
      val blinked = blink(acc)
      println(s"Blinked $index times")
      blinked
    )
    println(manyBlinks.size)

  extension (m: mutable.Map[String, Long])
    def updateOrReplace(key: String, value: Long): Unit =
      m.get(key) match
        case Some(existing) => m(key) = existing + value
        case None => m(key) = value

  @main def partTwo(args: String*): Unit =
    val unparsedStones = readResource("input.txt")
    val stones = unparsedStones.split(" ").toSeq
    val stoneMap: mutable.Map[String, Long] = mutable.Map()
    stones.foreach(stoneMap.updateOrReplace(_, 1))
    val finalMap = (1 to 75).foldLeft(stoneMap)((acc, index) =>
        val newMap: mutable.Map[String, Long] = mutable.Map()
        acc.keys.foreach { key =>
          val count = acc.getOrElse(key , 0L)
          key match
            case "0" => newMap.updateOrReplace("1", count)
            case stone if stone.length % 2 == 0 =>
              val (leftStone, rightStone) = stone.splitAt(stone.length / 2)
              newMap.updateOrReplace(dropLeadingZeroes(leftStone), count)
              newMap.updateOrReplace(dropLeadingZeroes(rightStone), count)
            case stone => newMap.updateOrReplace((stone.toLong * 2024).toString, count)
        }
        println(s"Blinked $index times")
        newMap
    )
    println(finalMap.values.sum)
