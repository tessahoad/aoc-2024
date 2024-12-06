package aoc.day5

import aoc.common.Utils

import scala.annotation.tailrec
import scala.util.matching.Regex

object Main extends Utils:

  extension[T] (list: Seq[T])
    def moveElement(fromIndex: Int, toIndex: Int): Seq[T] =
      val element = list(fromIndex)
      val withoutElement = list.take(fromIndex) ++ list.drop(fromIndex + 1)
      val (before, after) = withoutElement.splitAt(toIndex)
      before ++ (element +: after)

    def getMiddleElement: T =
      list(list.length / 2)

  extension (m: Map[String, Seq[String]])
    def updateOrReplace(key: String, value: Seq[String]): Map[String, Seq[String]] =
      m.get(key) match
        case Some(existing) => m + (key -> (existing ++ value))
        case None => m + (key -> value)

    def invert: Map[String, Seq[String]] =
      m.foldLeft(Map.empty[String, Seq[String]]) { case (acc, (key, values)) =>
        acc.updateOrReplace(key, values)
      }

  def isUpdateValid(update: Seq[String], rulesMap: Map[String, Seq[String]]): Boolean =
    update.zipWithIndex.forall { case (value, index) =>
      rulesMap.get(value).forall(rule => !update.take(index).exists(rule.contains))
    }

  @annotation.tailrec
  def cleanseUpdate(update: Seq[String], rulesMap: Map[String, Seq[String]], invertedMap: Map[String, Seq[String]]): Seq[String] = {
    val invalidIndex = update.zipWithIndex.indexWhere { case (entry, index) =>
      rulesMap.get(entry).exists(rule => update.take(index).exists(rule.contains))
    }

    if (invalidIndex == -1) update
    else {
      val entry = update(invalidIndex)
      val pagesToGoBefore = invertedMap(entry)
      val smallestIndex = pagesToGoBefore.map { page => update.indexOf(page) }.filterNot(_ == -1).min
      val cleansedUpdate = update.moveElement(invalidIndex, smallestIndex)
      cleanseUpdate(cleansedUpdate, rulesMap, invertedMap)
    }
  }

  @main def partOne(args: String*): Unit =
    val rawSafetyManual = readResourceLines("input.txt")
    val (rules, updates) = rawSafetyManual.partition(_.contains("|"))
    val rulesMap = rules
      .map(_.split("\\|"))
      .map(ruleList => (ruleList.head, ruleList.last))
      .foldLeft(Map.empty[String, Seq[String]]) { case (acc, (key, value)) =>
        acc.updateOrReplace(key, Seq(value))
      }
    val parsedUpdates = updates.filterNot(_.isEmpty).map(_.split(",").toSeq)
    val middlePages = parsedUpdates.collect {
      case update if isUpdateValid(update, rulesMap) => update.getMiddleElement
    }
    print(middlePages.map(_.toInt).sum)

  @main def partTwo(args: String*): Unit =
    val rawSafetyManual = readResourceLines("input.txt")
    val (rules, updates) = rawSafetyManual.partition(_.contains("|"))
    val rulesMap = rules
      .map(_.split("\\|"))
      .map(ruleList => (ruleList.head, ruleList.last))
      .foldLeft(Map.empty[String, Seq[String]]) { case (acc, (key, value)) =>
        acc.updateOrReplace(key, Seq(value))
      }
    val invertedMap = rulesMap.invert
    val parsedUpdates = updates.filterNot(_.isEmpty).map(_.split(",").toSeq)
    val invalidUpdates = parsedUpdates.filterNot { update =>
      isUpdateValid(update, rulesMap)
    }
    val validatedUpdates = invalidUpdates.map{ update =>
      cleanseUpdate(update, rulesMap, invertedMap).getMiddleElement.toInt
    }
    print(validatedUpdates.sum)








