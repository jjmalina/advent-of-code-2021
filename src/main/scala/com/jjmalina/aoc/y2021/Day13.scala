package com.jjmalina.aoc.y2021

import cats._
import cats.effect._
import cats.syntax.all._
import fs2._
import com.jjmalina.aoc.AOCApp
import cats.syntax.apply

object Day13 extends AOCApp(2021, 13) {
  case class Coordinate(x: Int, y: Int) {
    def applyFold(instruction: Coordinate): Coordinate = {
      instruction match {
        case Coordinate(place, 0) => if (x < place) this else (Coordinate(place - (x - place), y))
        case Coordinate(0, place) => if (y < place) this else (Coordinate(x, place - (y - place)))
        case _ => this
      }
    }
  }

  def parseInput(input: List[String]): (List[Coordinate], List[Coordinate]) = {
    input.foldRight((List.empty[Coordinate], List.empty[Coordinate])) {
      case (str, (coords, instructions)) => {
        str match {
          case s"$x,$y" => (Coordinate(x.toInt, y.toInt) :: coords, instructions)
          case s"fold along x=$x" => (coords, Coordinate(x.toInt, 0) :: instructions)
          case s"fold along y=$y" => (coords, Coordinate(0, y.toInt) :: instructions)
          case _ => (coords, instructions)
        }
      }
    }
  }

  def applyFolds(
    coordinates: List[Coordinate],
    instructions: List[Coordinate]
  ): Set[Coordinate] = {
    instructions.foldLeft(coordinates.toSet) {
      case (coordinates, instruction) => {
        coordinates.map(c => c.applyFold(instruction)).toSet
      }
    }
  }

  def outputFinal(coordinates: List[Coordinate], instructions: List[Coordinate]): String = {
    val finalCoordinates = applyFolds(coordinates, instructions)
    val maxX = finalCoordinates.map(_.x).max
    val maxY = finalCoordinates.map(_.y).max
    val lastXFold = instructions.filter(_.x != 0).map(_.x).lastOption.getOrElse(maxX + 1)
    val lastYFold = instructions.filter(_.y != 0).map(_.y).lastOption.getOrElse(maxY + 1)
    (0 until lastYFold).map(
      y => (0 until lastXFold).map(
        x => {
          if (finalCoordinates.contains(Coordinate(x, y))) "#" else "."
        }
      ).mkString
    ).mkString("\n")
  }

  override def part1(input: Stream[IO, String]): IO[String] =
    input.through(text.lines).compile.toList.map(lines => {
      val (coords, instructions) = parseInput(lines)
      applyFolds(coords, instructions.take(1)).size.toString
    })
  override def part2(input: Stream[IO, String]): IO[String] =
    input.through(text.lines).compile.toList.map(lines => {
      val (coords, instructions) = parseInput(lines)
      outputFinal(coords, instructions)
    })
}
