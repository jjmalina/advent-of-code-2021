package com.jjmalina.aoc.y2021

import cats._
import cats.effect._
import cats.syntax.all._
import fs2._
import com.jjmalina.aoc.AOCApp

object Day06 extends AOCApp(2021, 6) {

  def growFish(initialFish: List[(Long,Long)], days: Int): List[(Long,Long)] = {
    (1 to days).foldLeft(initialFish)((currentFish, day) => {
      val fishToAdd = currentFish.filter(_._2 == 0).map(_._1).sum
      val newFishState = currentFish.map((school) => {
        val (count, days) = school
        val nextDay = if (days == 0) 6 else (days - 1)
        (count, nextDay)
      })
      if (fishToAdd > 0) {
        newFishState ++ List((fishToAdd, 8))
      } else {
        newFishState
      }
    })
  }

  def solve(input: Stream[IO, String], days: Int): IO[String] =
    input.through(text.lines).filter(_.length > 0).compile.toList.map(line => {
      val fishDays = line.head.split(",").map(_.toLong).toList
      val initialState = fishDays.map(d => (1L, d.toLong))
      val endState = growFish(initialState, days)
      endState.map(_._1).sum.toString
    })
  override def part1(input: Stream[IO, String]): IO[String] =
    solve(input, 80)

  override def part2(input: Stream[IO, String]): IO[String] =
    solve(input, 256)
}
