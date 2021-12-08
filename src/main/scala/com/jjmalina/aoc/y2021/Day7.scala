package com.jjmalina.aoc.y2021

import cats._
import cats.effect._
import cats.syntax.all._
import fs2._
import com.jjmalina.aoc.AOCApp

object Day07 extends AOCApp(2021, 7) {
  def calculateTotalFuel(positions: List[Int], dest: Int, fuelCostFn: Int => Int): Int =
    positions.map(p => if (p > dest) fuelCostFn(p - dest) else fuelCostFn(dest - p)).sum

  def solve(positions: List[Int], fuelCostFn: Int => Int): Int = {
    val max = positions.max
    (0 to max).map(calculateTotalFuel(positions, _, fuelCostFn)).min
  }

  def crabFuelCost(distance: Int): Int = (1 to distance).sum

  def positions(input: Stream[IO, String]): IO[List[Int]] =
    input.through(text.lines).filter(_.length > 0).compile.toList.map(
      lines => lines.head.split(",").map(_.toInt).toList
    )

  override def part1(input: Stream[IO, String]): IO[String] =
    positions(input).map(solve(_, (a: Int) => a).toString)

  override def part2(input: Stream[IO, String]): IO[String] =
    positions(input).map(solve(_, crabFuelCost).toString)
}
