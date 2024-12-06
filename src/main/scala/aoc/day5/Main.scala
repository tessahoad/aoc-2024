package aoc.day5

import aoc.common.Utils

import scala.util.matching.Regex

object Main extends Utils:


  @main def partOne(args: String*): Unit =
    val rawSafetyManual = readResourceLines("input.txt")
    val (rules, updates) = rawSafetyManual.partition(_.contains("|"))
    val rulesMap = rules
      .map(_.split("\\|"))
      .map(ruleList => (ruleList.head, ruleList.last))
      .foldLeft(Map.empty[String, Seq[String]]) { case (acc, (key, value)) =>
        acc.get(key) match
          case Some(existing) => acc + (key -> (existing ++ Seq(value)))
          case None => acc + (key -> Seq(value))
      }
    val parsedUpdates = updates.filterNot(_.isEmpty).map(_.split(",").toSeq)
    val middlePages = parsedUpdates.map { update =>
      val ruleBreaking = update.zipWithIndex.map { case (value, index) =>
        rulesMap.get(value) match
          case Some(rule) =>
            !update.take(index).exists(rule.contains)
          case None => true
      }
      if !ruleBreaking.contains(false) then
        update(update.length / 2)
      else
        "0"
    }
    print(middlePages.map(_.toInt).sum)








