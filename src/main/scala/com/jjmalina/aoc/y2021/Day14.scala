package com.jjmalina.aoc.y2021

import cats._
import cats.effect._
import cats.syntax.all._
import fs2._
import scala.collection.mutable
import com.jjmalina.aoc.AOCApp

object Day14 extends AOCApp(2021, 14) {
  def parseInput(input: List[String]): (List[Char], Map[List[Char], Char]) = {
    input.foldRight((List.empty[Char], Map.empty[List[Char], Char])) {
      case (line, (template, rules)) => {
        line match {
          case s"$pair -> $insertion" => (template, rules ++ Map(pair.toList -> insertion.charAt(0)))
          case s"$template" => (template.toList, rules)
          case _ => (template, rules)
        }
      }
    }
  }

  def applyInsertions(template: Vector[Char], insertionRules: Map[List[Char], Char]): Vector[Char] = {
    template.foldLeft(Vector.empty[Char]) {
      case (acc, current) => {
        acc match {
          case Nil => Vector(current)
          case _ => acc concat insertionRules.get(List(acc.last, current)).map(Vector(_, current)).getOrElse(Vector(current))
        }
      }
    }
  }

  def buildPolymer(n: Int, template: List[Char], insertionRules: Map[List[Char], Char]): Vector[Char] =
    (1 to n).foldRight(template.toVector) {
      case (step, currentPolymers) => {
        println((step, currentPolymers.length))
        applyInsertions(currentPolymers, insertionRules)
      }
    }

  // this was the naive solution before I realized that
  // dynamic programming was the only way to do part2 due to memory and stack constraints
  def solveViaBuilding(input: List[String], n: Int): Int = {
    val (template, instructions) = parseInput(input)
    val polymer = buildPolymer(n, template, instructions)
    val counts = Monoid.combineAll(polymer.map(c => Map(c -> 1)))
    counts.values.max - counts.values.min
  }

  // This function has a mutable Map
  // TODO: use a State monad to memoize instead
  def charCounts(
    left: Char,
    right: Char,
    depth: Int,
    insertionRules: Map[List[Char], Char],
    memo: mutable.Map[(List[Char], Int), Map[Char, Long]]
  ): Map[Char, Long] = {
    if (depth == 0) {
      Map.empty[Char, Long]
    } else if (memo.get((List(left, right), depth)).isDefined) {
      memo.get((List(left, right), depth)).get
    } else {
      insertionRules.get(List(left, right)).map(char => {
        val leftCounts = memo.get((List(left, char), depth - 1)) match {
          case Some(value) => value
          case None => {
            val counts = charCounts(left, char, depth - 1, insertionRules, memo)
            memo.put((List(left, char), depth - 1), counts)
            counts
          }
        }
        val rightCounts = memo.get((List(char, right), depth - 1)) match {
          case Some(value) => value
          case None => {
            val counts = charCounts(char, right, depth - 1, insertionRules, memo)
            memo.put((List(char, right), depth - 1), counts)
            counts
          }
        }
        val counts =  Monoid.combineAll(List(leftCounts, Map(char -> 1L), rightCounts))
        memo.put((List(left, right), depth), counts)
        counts
      }).getOrElse(Map.empty[Char, Long])
    }
  }

  def countChars(iterations: Int, template: List[Char], insertionRules: Map[List[Char], Char]): Map[Char, Long] = {
    val initialCounts = Monoid.combineAll(template.map(c => Map(c -> 1L)))
    val (_, counts) = template.foldLeft((Vector.empty[Char], initialCounts)) {
      case ((passed, acc), currentChar) => {
        passed match {
          case Nil => (Vector(currentChar), acc)
          case _ => (
            passed.appended(currentChar),
            Monoid.combine(
              acc,
              charCounts(passed.last, currentChar, iterations, insertionRules, mutable.Map.empty[(List[Char], Int), Map[Char, Long]]))
          )
        }
      }
    }
    Monoid.combine(initialCounts, counts)
  }

  def solveViaRecursion(input: List[String], n: Int): Long = {
    val (template, instructions) = parseInput(input)
    val counts = countChars(n, template, instructions)
    counts.values.max - counts.values.min
  }
  override def part1(input: Stream[IO, String]): IO[String] =
    input.through(text.lines).compile.toList.map(solveViaRecursion(_, 10).toString)
  override def part2(input: Stream[IO, String]): IO[String] =
    input.through(text.lines).compile.toList.map(solveViaRecursion(_, 40).toString)
}
