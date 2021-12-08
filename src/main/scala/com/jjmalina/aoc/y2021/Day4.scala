package com.jjmalina.aoc.y2021


import cats._
import cats.effect._
import cats.syntax.all._
import fs2._
import com.jjmalina.aoc.AOCApp

object Day04 extends AOCApp(2021, 4) {

  case class Board(
    numbers: List[List[Int]],
    markedNumbers: List[Tuple3[Int, Int, Int]] = List.empty
  ) {
    def playNumber(number: Int): Board = {
      val newMarked = (0 until numbers.length).toList.foldLeft(List.empty[Tuple3[Int, Int, Int]])((marked, rowNum) => {
        marked ++ numbers(rowNum).zipWithIndex.toList.filter(_._1 == number).map {
          case (_, colnum) => (rowNum, colnum, number)
        }
      })
      this.copy(markedNumbers = markedNumbers ++ newMarked)
    }

    def isWinner: Boolean = {
      /* slow implementation but whatever for now*/
      val horizontalWinner = (0 until numbers.length).toList.exists(rowNum => {
        (0 until numbers.length).toList.forall(colNum => markedNumbers.exists(
          n => n === (rowNum, colNum, numbers(rowNum)(colNum))
        ))
      })
      val verticalWinner = (0 until numbers.length).toList.exists(colNum => {
        (0 until numbers.length).forall(rowNum => markedNumbers.exists(
          n => n === (rowNum, colNum, numbers(rowNum)(colNum))
        ))
      })
      horizontalWinner || verticalWinner
    }

    def lastNumberPlayed: Option[Int] = markedNumbers.lastOption.map(_._3)
    def unmarkedNumbers: List[Int] = {
      (0 until numbers.length).toList.flatMap(rowNum => {
        (0 until numbers.length).filter(colNum => {
          !markedNumbers.exists(n => n._1 == rowNum && n._2 == colNum)
        }).map(numbers(rowNum)(_))
      })
    }

    def score: Option[Int] = lastNumberPlayed.map(_ * unmarkedNumbers.sum)
  }

  def constructBoards(input: List[List[String]]): List[Board] = {
    input.map(boardLines => {
      Board(boardLines.map(_.split(" +").filter(_.length > 0).map(_.toInt).toList).toList)
    })
  }

  def playUntilWinner(drawnNumbers: List[Int], boards: List[Board]): Tuple2[List[Board], List[Board]] = {
    drawnNumbers.foldLeft((List.empty[Board], boards))((current, number) => {
      val (existingWinners, currentBoards) = current
      if (existingWinners.length > 0) {
        (existingWinners, currentBoards)
      } else {
        val newState = currentBoards.map(_.playNumber(number))
        (newState.filter(_.isWinner), newState)
      }
    })
  }

  def playUntilAllWinners(drawnNumbers: List[Int], boards: List[Board]): Tuple2[List[Board], List[Board]] = {
    drawnNumbers.foldLeft((List.empty[Board], boards))((current, number) => {
      val (existingWinners, currentBoards) = current
      val newBoardState = currentBoards.map(_.playNumber(number))
      (
        existingWinners.concat(newBoardState.filter(_.isWinner)),
        newBoardState.filter(!_.isWinner)
      )
    })
  }

  def getInitialState(input: Stream[IO, String]): IO[Tuple2[List[Int], List[Board]]] = {
    input.through(text.lines)
      .fold((List.empty[Int], List.empty[List[String]], List.empty[String]))((acc, line) => {
        acc match {
          case (List(), b@List(), c@List()) => (line.split(",").map(_.toInt).toList, b, c)
          case (numbers, boards, currentBoard) => line match {
            case "" => (numbers, boards :+ currentBoard, List.empty[String])
            case line => (numbers, boards, currentBoard :+ line)
          }
        }
      })
      .compile.toList.map(_.head).map(boardInputs => {
        val (drawnNumbers, boardLines, _) = boardInputs
        (drawnNumbers, constructBoards(boardLines))
      })
  }

  override def part1(input: Stream[IO, String]): IO[String] =
    getInitialState(input).map(boardInputs => {
      val (drawnNumbers, boards) = boardInputs
      val (winners, endState) = playUntilWinner(drawnNumbers, boards)
      winners.headOption.flatMap(_.score).map(_.toString).getOrElse("no answer")
    })

  override def part2(input: Stream[IO, String]): IO[String] =
    getInitialState(input).map(boardInputs => {
      val (drawnNumbers, boards) = boardInputs
      val (winners, endState) = playUntilAllWinners(drawnNumbers, boards)
      winners.lastOption.flatMap(_.score).map(_.toString).getOrElse("no answer")
    })
}
