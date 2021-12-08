package com.jjmalina.aoc.y2021

import cats._
import cats.effect._
import cats.syntax.all._
import fs2._
import com.jjmalina.aoc.AOCApp

object Day05 extends AOCApp(2021, 5) {

  case class Line(x1: Int, y1: Int, x2: Int, y2: Int) {
    def isHorizontal = x1 == x2
    def isVertical = y1 == y2
    def isDiagonal = !isHorizontal && !isVertical
    def points: List[Tuple2[Int, Int]] = {
      if (isHorizontal) {
        val step = if (y1 < y2) 1 else -1
        (y1 to y2 by step).map(y => (x1, y)).toList
      } else if (isVertical) {
        val step = if (x1 < x2) 1 else -1
        (x1 to x2 by step).map(x => (x, y1)).toList
      } else {
        val xStep = if (x1 < x2) 1 else -1
        val yStep = if (y1 < y2) 1 else -1
        List((x1, y1)) ++ List.unfold((x1, y1))(curr => {
          if (curr == (x2, y2)) {
            None
          } else {
            val (x, y) = curr
            val next = (x + xStep, y + yStep)
            Some((next, next))
          }
        })
      }
    }
  }
  object Line {
    /* TODO monadic failure case handling */
    def apply(s: String): Line = {
      val parts = s.split(" -> ")
      val coords = parts.flatMap(_.split(",")).map(_.toInt)
      Line(coords(0), coords(1), coords(2), coords(3))
    }
  }

  def lines(input: Stream[IO, String]): Stream[IO, Line] =
    input
      .through(text.lines)
      .filter(_.length > 0)
      .map(Line(_))

  def countOverlappingPoints(points: Stream[IO, Tuple2[Int,Int]]): IO[Int] =
    points.fold(Map.empty[Tuple2[Int,Int], Int])((numbers, point) => {
      numbers + (point -> (numbers.get(point).getOrElse(0) + 1))
    }).compile.toList.map(_.head).map(pointCounts => {
      pointCounts.filter(_._2 >= 2).size
    })

  override def part1(input: Stream[IO, String]): IO[String] =
    countOverlappingPoints(
      lines(input)
        .filter(!_.isDiagonal)
        .flatMap(line => Stream.emits(line.points))
    ).map(_.toString)

  override def part2(input: Stream[IO, String]): IO[String] =
    countOverlappingPoints(
      lines(input).flatMap(line => Stream.emits(line.points))
    ).map(_.toString)
}
