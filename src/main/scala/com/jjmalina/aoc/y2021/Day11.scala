package com.jjmalina.aoc.y2021

import cats._
import cats.effect._
import cats.syntax.all._
import fs2._
import com.jjmalina.aoc.AOCApp

object Day11 extends AOCApp(2021, 11) {
  def octoMap(input: List[String]): Vector[Vector[Int]] =
    input.toVector.map(_.map(c => Integer.parseInt(c.toString)).toVector)

  def applyStep(om: Vector[Vector[Int]]) = om.map(_.map(_ + 1))
  def neighbors(om: Vector[Vector[Int]], row: Int, col: Int): List[(Int, Int)] = {
    val height = om.length
    val width = om.head.length
    val left = if (col - 1 < 0) None else Some((row, col - 1))
    val right = if (col + 1 > width - 1) None else Some((row, col + 1))
    val top = if (row - 1 < 0) None else Some((row - 1, col))
    val topLeft = if (row - 1 >= 0 && col - 1 >= 0) Some((row - 1, col - 1)) else None
    val topRight = if (row - 1 >= 0 && col + 1 <= width - 1) Some((row - 1, col + 1)) else None
    val bottom = if (row + 1 > height - 1) None else Some((row + 1, col))
    val bottomLeft = if (row + 1 <= height - 1 && col - 1 >= 0) Some((row + 1, col - 1)) else None
    val bottomRight = if (row + 1 <= height - 1 && col + 1 <= width - 1) Some((row + 1, col +1)) else None
    List(topLeft, top, topRight, left, right, bottomLeft, bottom, bottomRight).flatten
  }

  def findFlashCoordinates(om: Vector[Vector[Int]]): List[(Int,Int)] =
    (0 until om.length).toList.flatMap(
      row => (0 until om.head.length).toList.flatMap(
        col => if (om(row)(col) > 9) List((row, col)) else List.empty[(Int, Int)]
      )
    )

  def findFlashes(om: Vector[Vector[Int]]): (Set[(Int, Int)], Vector[Vector[Int]]) = {
    val flashIterations = List.unfold((Set.empty[(Int,Int)], om)) {
      case (existingFlashes, currentOctos) => {
        // first find flashes in the current board
        val flashes = findFlashCoordinates(currentOctos).toSet
        if (flashes.size == 0) {
          // if there are no flashes, terminate
          None
        } else {
          val currentFlashes = existingFlashes ++ flashes
          // get the neighbors of the new flashes (if they're not flashes already)
          // and generate their coordinates
          // if they appear more than once aggregate them to know how much to increment by
          val neighborIncrements = Monoid.combineAll(flashes.toList.flatMap(
            flash => neighbors(
              currentOctos, flash._1, flash._2
            ).filter(!currentFlashes.contains(_)).map(c => Map(c -> 1))
          ))
          val newOctos = (0 until currentOctos.length).toVector.map(
            row => (0 until currentOctos.head.length).toVector.map(col => {
              val coord = (row, col)
              if (currentFlashes.contains(coord)) {
                0
              } else {
                neighborIncrements
                  .get(coord)
                  .map(_ + currentOctos(row)(col))
                  .getOrElse(currentOctos(row)(col))
              }
            })
          )
          Some(((currentFlashes, newOctos), (currentFlashes, newOctos)))
        }
      }
    }
    // then take the last item and return it
    flashIterations.lastOption.getOrElse((Set.empty[(Int,Int)], om))
  }

  def isSimultaneousFlash(om: Vector[Vector[Int]]): Boolean =
    (0 until om.length).forall(row => (0 until om.head.length).forall(col => om(row)(col) == 0))

  def simulateFlashes(om: Vector[Vector[Int]], steps: Int): Int = {
    val (count, _) = (1 to steps).foldRight((0, om)) {
      case (step, (count, currentOctoMap)) => {
        val preFlashOctos = applyStep(currentOctoMap)
        val (flashCoords, newOctos) = findFlashes(preFlashOctos)
        (count + flashCoords.size, newOctos)
      }
    }
    count
  }

  def simulation(om: Vector[Vector[Int]]): Stream[Pure, (Int,Vector[Vector[Int]])] = {
    Stream.unfold((0, om)) {
      case (step, currentOctos) => {
        val preFlashOctos = applyStep(currentOctos)
        val (flashCoords, newOctos) = findFlashes(preFlashOctos)
        val record = (flashCoords.size, newOctos)
        Some((record, record))
      }
    }
  }

  override def part1(input: Stream[IO, String]): IO[String] =
    input
      .through(text.lines)
      .filter(_.length > 0)
      .compile
      .toList
      .map(l => {
        val octoSteps = simulation(octoMap(l))
        octoSteps.take(100).map(_._1).toList.sum.toString
      })

  override def part2(input: Stream[IO, String]): IO[String] =
    input
      .through(text.lines)
      .filter(_.length > 0)
      .compile
      .toList
      .map(l => {
        val octoSteps = simulation(octoMap(l)).zipWithIndex
        octoSteps
          .filter(elem => isSimultaneousFlash(elem._1._2))
          .take(1)
          .toList.headOption.map(elem => (elem._2 + 1).toString).getOrElse("no answer")
      })
}
