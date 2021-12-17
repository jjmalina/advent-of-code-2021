package com.jjmalina.aoc.y2021

import cats._
import cats.effect._
import cats.syntax.all._
import fs2._
import scala.collection.mutable
import com.jjmalina.aoc.AOCApp

object Day15 extends AOCApp(2021, 15) {
  def parseInput(input: List[String]): Vector[Vector[Double]] =
    input.toVector.map(_.map(c => c.toString.toDouble).toVector)

  def validPositions(
    place: (Int, Int),
    cave: Vector[Vector[Double]],
    visited: mutable.Set[(Int, Int)]
  ): Set[(Int, Int)] = {
    val lastRowIndex = cave.length - 1;
    val lastColIndex = cave.head.length - 1;
    val (row, col) = place
    if ((row, col) == (lastRowIndex, lastColIndex)) {
      return Set.empty[(Int, Int)]
    } else {
      Set(
        // top
        if (row - 1 < 0 || visited.contains((row - 1, col))) None else Some((row - 1, col)),
        // left
        if (col - 1 < 0 || visited.contains((row, col - 1))) None else Some((row, col - 1)),
        // right
        if (col + 1 > lastColIndex || visited.contains((row, col + 1))) None else Some((row, col + 1)),
        // bottom
        if (row + 1 > lastRowIndex || visited.contains((row + 1, col))) None else Some((row + 1, col))
      ).flatten.filter(_ != (0, 0))
    }
  }

  def printDistances(lastRow: Int, lastCol: Int, dist: mutable.Map[(Int,Int), Double]): Unit = {
    println("\n\n")
    (0 to lastRow).foreach(row => {
      println((0 to lastCol).map(col => {
        val value = dist((row,col))
        if (value == Double.PositiveInfinity) {
          "X"
        } else {
          "1"
        }
      }).mkString)
    })
    println("\n\n")
  }

  def lowestRiskThroughCave(cave: Vector[Vector[Double]]): Double = {
    // This is Dijkstra's algorithm which I adapted from
    // https://medium.com/se-notes-by-alexey-novakov/algorithms-in-scala-dijkstra-shortest-path-78c4291dd8ab
    val lastRow = cave.length - 1
    val lastCol = cave.head.length - 1
    val total = cave.length * cave.head.length
    val start = (0, 0)
    val previousEdge = mutable.Map[(Int,Int), (Int,Int)]()
    val distanceFromStart = mutable.Map.from(
      (0 to lastRow).flatMap(
        row => (0 to lastCol).map(col => ((row, col), Double.PositiveInfinity))
      )
    )
    // val visited = mutable.Set.empty[(Int,Int)]
    distanceFromStart.put(start, 0.0)
    val sortByDistance: Ordering[((Int,Int), Double)] = (a, b) => a._2.compareTo(b._2)
    val queue = mutable.PriorityQueue[((Int,Int), Double)]((start, 0.0))(sortByDistance)
    val nodesInQueue = mutable.Set(start)
    var count = 0
    while (queue.nonEmpty) {
      val (current, _) = queue.dequeue()
      nodesInQueue.remove(current)
      val riskLevelFromStart = distanceFromStart(current)
      count += 1
      val neighbors = validPositions(current, cave, mutable.Set.empty[(Int, Int)])
      neighbors.foreach(neighbor => {
        val neighborRisk = cave(neighbor._1)(neighbor._2)
        val neighborRiskFromStart = riskLevelFromStart + neighborRisk
        if (distanceFromStart(neighbor) > neighborRiskFromStart) {
          distanceFromStart.put(neighbor, neighborRiskFromStart)
          previousEdge.put(neighbor, current)
          if (!nodesInQueue.contains(neighbor)) {
            queue.enqueue((neighbor, neighborRiskFromStart))
            nodesInQueue.add(neighbor)
          }
        }
      })
    }
    // printDistances(lastRow, lastCol, distanceFromStart)
    distanceFromStart((lastRow, lastCol))
  }

  def incrementValue(originalValue: Double, increment: Double): Double =
    if (originalValue + increment > 9) (originalValue + increment) - 9 else originalValue + increment

  def enlargeCave(cave: Vector[Vector[Double]]): Vector[Vector[Double]] = {
    val sectorIncrements = (0 to 4).map(row => (0 to 4).map(col => (row + col).toDouble))
    val originalColumns = cave.head.length
    val newCols = originalColumns * 5
    val newRows = cave.length * 5
    println((newRows, newCols))
    (0 until newRows).map(
      row => (0 until newCols).map(col => {
        val originalCaveRow = row % originalColumns
        val originalCaveCol = col % originalColumns
        val originalCaveValue = cave(row % originalColumns)(col % originalColumns)
        val increment = sectorIncrements(row / originalColumns)(col / originalColumns)
        incrementValue(originalCaveValue, increment)
      }).toVector
    ).toVector
  }

  override def part1(input: Stream[IO, String]): IO[String] =
    input.through(text.lines).filter(_.length > 0).compile.toList.map(
      lines => lowestRiskThroughCave(parseInput(lines)).toInt.toString
    )

  override def part2(input: Stream[IO, String]): IO[String] =
    input.through(text.lines).filter(_.length > 0).compile.toList.map(
      lines => lowestRiskThroughCave(enlargeCave(parseInput(lines))).toInt.toString
    )
}
