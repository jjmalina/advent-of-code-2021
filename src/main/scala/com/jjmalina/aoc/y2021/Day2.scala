package com.jjmalina.aoc.y2021

import cats._
import cats.effect._
import cats.syntax.all._
import fs2._
import com.jjmalina.aoc.AOCApp

object Day02 extends AOCApp(2021, 2) {
  def commandsWithValues(input: Stream[IO, String]): Stream[IO, (String, Int)] =
    input
      .through(text.lines)
      .filter(_.length > 0)
      .map(line => {
        val data = line.split(" ")
        (data(0), data(1).toInt)
      })

  override def part1(input: Stream[IO, String]): IO[String] = {
    commandsWithValues(input)
      .fold((0, 0)) {
        case ((horizontal, depth), (command, value)) => {
          command match {
            case "forward" => (horizontal + value, depth)
            case "up" => (horizontal, depth - value)
            case "down" => (horizontal, depth + value)
          }
        }
      }
      .map {
        case (horizontal, depth) => horizontal * depth
      }
      .compile
      .toList
      .map(_.head.toString)
  }

  override def part2(input: Stream[IO, String]): IO[String] = {
    commandsWithValues(input)
      .fold((0, 0, 0)) {
        case ((horizontal, depth, aim), (command, value)) => {
          command match {
            case "forward" => (horizontal + value, depth + (aim * value), aim)
            case "up" => (horizontal, depth, aim - value)
            case "down" => (horizontal, depth, aim + value)
          }
        }
      }
      .map {
        case (horizontal, depth, _) => horizontal * depth
      }
      .compile
      .toList
      .map(_.head.toString)
  }
}
