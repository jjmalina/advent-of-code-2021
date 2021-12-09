package com.jjmalina.aoc.y2021

import cats._
import cats.effect._
import cats.syntax.all._
import fs2._
import com.jjmalina.aoc.AOCApp
import scala.annotation.tailrec

object Day09 extends AOCApp(2021, 9) {
  case class HeightMap(rows: Vector[Vector[Int]], height: Int, width: Int) {
    def neighborCoordinates(row: Int, col: Int): List[(Int, Int)] = {
      val up = if (row - 1 < 0) None else Some((row - 1, col))
      val down = if (row + 1 > height - 1) None else Some((row + 1, col))
      val left = if (col - 1 < 0) None else Some((row, col - 1))
      val right = if (col + 1 > width - 1) None else Some((row, col + 1))
      List(up, down, left, right).flatten
    }

    def lowPointCoordinates: List[(Int, Int)] = {
      val mapCoordinates = (0 until height).flatMap(h => (0 until width).map((h, _))).toList
      mapCoordinates.filter {
        case (row, col) => {
          val value = rows(row)(col)
          neighborCoordinates(row, col).forall(nc => value < rows(nc._1)(nc._2))
        }
      }
    }
    def lowPointValues: List[Int] = lowPointCoordinates.map(c => rows(c._1)(c._2))
    def riskLevels: List[Int] = lowPointValues.map(_ + 1)
    def getBasin(seen: Set[(Int,Int)], row: Int, col: Int): List[(Int, Int)] = {
      val currentValue = rows(row)(col)
      val neighborsInBasin = neighborCoordinates(row, col).filter {
        case (r, c) => {
          val v = rows(r)(c)
          v != 9 && v >= currentValue && !seen.contains((r, c))
        }
      }
      val newSeen = seen ++ Set((row, col))
      (Set((row, col)) ++ neighborsInBasin.flatMap(c => getBasin(newSeen, c._1, c._2)).toSet).toList
    }
    def basins: List[List[(Int, Int)]] =
      lowPointCoordinates.map(c => getBasin(Set.empty[(Int,Int)], c._1, c._2))
  }
  object HeightMap {
    def apply(input: List[String]): HeightMap = {
      val height = input.length
      val width = input.head.length
      val rows = input.map(row => row.split("").toVector.map(_.toInt)).toVector
      HeightMap(rows, height, width)
    }
  }

  override def part1(input: Stream[IO, String]): IO[String] =
    input.through(text.lines).filter(_.length > 0).compile.toList
      .map(HeightMap(_).riskLevels.sum.toString)
  override def part2(input: Stream[IO, String]): IO[String] =
    input.through(text.lines).filter(_.length > 0).compile.toList
      .map(HeightMap(_).basins.map(_.length).sortBy(-_).take(3).reduce(_ * _).toString)
}
