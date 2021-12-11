package com.jjmalina.aoc.y2021

import cats._
import cats.effect._
import cats.syntax.all._
import fs2._
import com.jjmalina.aoc.AOCApp
import scala.collection.immutable

object Day10 extends AOCApp(2021, 10) {


  def validClosing(o: Char, c: Char): Boolean =
    (
      (getClosing(o) == c) ||
      (getClosing(o) == c) ||
      (getClosing(o) == c) ||
      (getClosing(o) == c)
    )
  def isOpening(c: Char): Boolean = c == '(' || c == '[' || c == '{' || c == '<'
  def isClosing(c: Char): Boolean = c == ')' || c == ']' || c == '}' || c == '>'
  def getClosing(c: Char): Char = c match {
    case '(' => ')'
    case '[' => ']'
    case '{' => '}'
    case '<' => '>'
    case _ => throw new Exception(s"Invalid char $c")
  }

  def findIllegal(s: String): (List[Char], Option[Char]) = {
    def loop(stack: List[Char], remaining: List[Char]): (List[Char], Option[Char]) = {
      remaining match {
        case currentChar :: remainingChars => {
          if (isOpening(currentChar)) {
            loop(currentChar :: stack, remainingChars)
          } else if (isClosing(currentChar)) {
            stack match {
              case lastOpening :: remainingOpening =>  {
                if (validClosing(lastOpening, currentChar)) {
                  loop(remainingOpening, remainingChars)
                } else {
                  (stack, Some(currentChar))
                }
              }
              case Nil => (stack, Some(currentChar))
            }
          } else {
            throw new Exception(s"Invalid char ${currentChar}")
          }
        }
        case Nil => (stack, None) // nothing left
      }
    }
    loop(List.empty[Char], s.toList)
  }

  def completeStack(stack: List[Char]): List[Char] =
    stack.foldLeft(List.empty[Char]) {
      case (chars, c) => chars.appended(getClosing(c))
    }

  val errorPoints = Map(
    ')' -> 3,
    ']' -> 57,
    '}' -> 1197,
    '>' -> 25137
  )
  override def part1(input: Stream[IO, String]): IO[String] =
    input
      .through(text.lines)
      .filter(_.length > 0)
      .map(findIllegal(_)._2.map(c => Map(c -> 1)))
      .foldMonoid
      .map(_.map(m => m.map({
        case (key: Char, count: Int) => errorPoints.get(key).map(_ * count).getOrElse(0)
      }).toList.sum).getOrElse(0))
      .compile
      .toList
      .map(_.head.toString)

  val completionPoints = Map(
    ')' -> 1L,
    ']' -> 2L,
    '}' -> 3L,
    '>' -> 4L
  )
  override def part2(input: Stream[IO, String]): IO[String] =
    input
      .through(text.lines)
      .filter(_.length > 0)
      .map(findIllegal(_))
      .filter(_._2.isEmpty)
      .map {
        case (stack, _) => completeStack(stack).foldLeft(0L) {
          case (score, char) => {
            val newScore = score * 5L
            newScore + completionPoints.get(char).getOrElse(0L)
          }
        }
      }
      .compile
      .toList
      .map(scores => {
        val sorted = scores.sorted
        sorted((sorted.length / 2)).toString
      })
}
