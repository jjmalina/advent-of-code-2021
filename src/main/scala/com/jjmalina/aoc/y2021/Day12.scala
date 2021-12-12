package com.jjmalina.aoc.y2021

import cats._
import cats.effect._
import cats.syntax.all._
import fs2._
import com.jjmalina.aoc.AOCApp

object Day12 extends AOCApp(2021, 12) {
  case class Cave(name: String) {
    def isStart: Boolean = name == "start"
    def isEnd: Boolean = name == "end"
    def isBigCave: Boolean = name.toUpperCase == name
    def isSmallCave: Boolean = name.toLowerCase == name
  }

  def parseCaveLine(input: String): (Cave, Cave) = {
    val caveStrings = input.split("-")
    (Cave(caveStrings(0)), Cave(caveStrings(1)))
  }

  def parseCaves(input: List[String]): List[(Cave, Cave)] =
    input.map(parseCaveLine)

  def caveMap(input: List[String]): Map[Cave, Set[Cave]] =
    Monoid.combineAll(parseCaves(input).map {
      case (leftCave, rightCave) => {
        Map(leftCave -> Set(rightCave), rightCave -> Set(leftCave))
      }
    })

  def spelunk(
    currentPath: List[Cave],
    currentCave: Cave,
    caves: Map[Cave, Set[Cave]]
  ): List[List[Cave]] = {
    val validConnections = caves.get(currentCave).map(
      connections => connections.filter(c => !currentPath.contains(c) || c.isBigCave)
    ).getOrElse(Set.empty[Cave])
    if (validConnections.size == 0 || currentCave.isEnd) {
      List(currentPath)
    } else {
      validConnections.toList.flatMap(
        c => spelunk(currentPath :+ c, c, caves)
      )
    }
  }

  def canVisitSmallCave(cave: Cave, visitedCaves: List[Cave]): Boolean = {
    val visitedSmallCaves = visitedCaves.filter(_.isSmallCave)
    val visitedSmallCaveCounts = Monoid.combineAll(visitedSmallCaves.map(c => Map(c -> 1)))
    if (visitedSmallCaveCounts.values.exists(_ == 2)) !visitedSmallCaves.contains(cave) else true
  }

  def spelunk2(
    currentPath: List[Cave],
    currentCave: Cave,
    caves: Map[Cave, Set[Cave]]
  ): List[List[Cave]] = {
    val validConnections = caves.get(currentCave).map(
      connections => connections.filter(
        c => !c.isStart && (c.isBigCave || canVisitSmallCave(c, currentPath))
      )
    ).getOrElse(Set.empty[Cave])
    if (validConnections.size == 0 || currentCave.isEnd) {
      List(currentPath)
    } else {
      validConnections.toList.flatMap(
        c => spelunk2(currentPath :+ c, c, caves)
      )
    }
  }

  def findValidPaths(caves: Map[Cave, Set[Cave]]): List[List[Cave]] =
    spelunk(List(Cave("start")), Cave("start"), caves)
      .filter(_.lastOption.map(_ == Cave("end")).getOrElse(false))

  def findValidPaths2(caves: Map[Cave, Set[Cave]]): List[List[Cave]] =
    spelunk2(List(Cave("start")), Cave("start"), caves)
      .filter(_.lastOption.map(_ == Cave("end")).getOrElse(false))

  override def part1(input: Stream[IO, String]): IO[String] =
    input.through(text.lines).filter(_.length > 0).compile.toList.map(
      input => findValidPaths(caveMap(input)).length.toString
    )
  override def part2(input: Stream[IO, String]): IO[String] =
    input.through(text.lines).filter(_.length > 0).compile.toList.map(
      input => findValidPaths2(caveMap(input)).length.toString
    )
}
