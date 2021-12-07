package com.jjmalina.aoc.y2021

import cats._
import cats.effect._
import cats.syntax.all._
import fs2._
import com.jjmalina.aoc.AOCApp

object Day01 extends AOCApp(2021, 1) {

  def solve(input: Stream[IO, String], groupSize: Int): IO[String] = {
    input
      .through(text.lines)
      .filter(_.length > 0)
      .map(_.toInt)
      .sliding(groupSize)
      .map(_.toList.sum)
      .fold((Option.empty[Int], 0))((acc, datum) => {
        val (previous, increased) = acc
        previous match {
          case Some(prev) => (Some(datum), if (datum > prev) increased + 1 else increased)
          case None => (Some(datum), increased)
        }
      })
      .map(_._2.toString)
      .compile
      .toList
      .map(_.head)
  }

  override def part1(input: Stream[IO, String]): IO[String] =
    solve(input, 1)

  override def part2(input: Stream[IO, String]): IO[String] =
    solve(input, 3)

}
