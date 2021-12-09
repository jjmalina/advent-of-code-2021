package com.jjmalina.aoc.y2021

import cats._
import cats.effect._
import cats.syntax.all._
import fs2._
import com.jjmalina.aoc.AOCApp

object Day08 extends AOCApp(2021, 8) {
  val identifiablePatternsByLength = Map(
    2 -> "cf",      // 1
    4 -> "bcdf",    // 4
    3 -> "acf",     // 7
    7 -> "abcdefg"  // 8
  )
  case class Signal(inputs: List[String], outputs: List[String]) {
    def isIdentifiable(signature: String): Boolean =
      identifiablePatternsByLength.contains(signature.length)

    def unidentifiableInputs: List[String] = inputs.filter(!isIdentifiable(_))
    def identifiableInputs: List[String] = inputs.filter(isIdentifiable(_))
    def identifiableOutputs: List[String] = outputs.filter(isIdentifiable(_))

    /* TODO handle failure */
    /*
      One way to deduce the signals is convert the segments to sets and take their differences
      The difference between segment sets of certain numbers is unique, so we can use process
      of elimination.

      I discovered this by drawing out the segments for each digit and taking the difference.

      First find 3 by subtracting the set of segments representing 7 from each unknown number
        the set of segments that is three should have length 2 when removing 7

      find 6 by searching through unknown numbers and subtracint 7's segment set from their segment sets
        the resulting difference with length 4 should correspond to the set of segments for 6

      find 0 by searching through unknown numbers and subtracting 3's sgement set
        the resulting difference should be length 3

      we now have 1, 4, 7, 8 and 3, 6, 0, which leaves 2, 5, 9

      find 5 by subtracting 6's segment set, which should give length zero

      now we just have 2 and 9 left
      find 2 by subtracting 5's segment set which should be length 2

      9 can be found be process of elimination

      we can then create a map of segment set -> digit string
      then loop through the output segment sets and find their digit

    */
    def outputValue: Int = {
      val oneSegmentSet = identifiableInputs.find(_.length == 2).get.toSet
      val fourSegmentSet = identifiableInputs.find(_.length == 4).get.toSet
      val sevenSegmentSet = identifiableInputs.find(_.length == 3).get.toSet
      val eightSegmentSet = identifiableInputs.find(_.length == 7).get.toSet

      val unidentifableSegmentSets = unidentifiableInputs.map(_.toSet)
      val threeSegmentSet = unidentifableSegmentSets.find(
        _.diff(sevenSegmentSet).size == 2).get
      val sixSegmentSet = unidentifableSegmentSets.find(
        _.diff(sevenSegmentSet).size == 4).get

      val remainingUnidentifiableSegmentSets = unidentifableSegmentSets.filter(s =>
        s != threeSegmentSet && s != sixSegmentSet
      )

      val zeroSegmentSet = remainingUnidentifiableSegmentSets.find(
        _.diff(threeSegmentSet).size == 2
      ).get

      val segmentSetsWithout036 = remainingUnidentifiableSegmentSets.filter(
        _ != zeroSegmentSet
      )
      val fiveSegmentSet = segmentSetsWithout036.find(_.diff(sixSegmentSet).size == 0).get
      val twoSegmentSet = segmentSetsWithout036
        .filter(_ != fiveSegmentSet)
        .find(_.diff(fiveSegmentSet).size == 2).get
      val nineSegmentSet = segmentSetsWithout036.filter(
        s => s != fiveSegmentSet && s != twoSegmentSet
      ).head

      val segmentSetToDigit = Map(
        zeroSegmentSet -> "0",
        oneSegmentSet -> "1",
        twoSegmentSet -> "2",
        threeSegmentSet -> "3",
        fourSegmentSet -> "4",
        fiveSegmentSet -> "5",
        sixSegmentSet -> "6",
        sevenSegmentSet -> "7",
        eightSegmentSet -> "8",
        nineSegmentSet -> "9"
      )

      Integer.parseInt(outputs.map(o => segmentSetToDigit(o.toSet)).mkString(""))
    }
  }
  object Signal {
    /* TODO handle failure */
    def apply(s: String): Signal = {
      val parts = s.split(" \\| ")
      Signal(parts(0).split(" ").toList, parts(1).split(" ").toList)
    }
  }

  def signals(input: Stream[IO, String]): Stream[IO, Signal] =
    input
      .through(text.lines)
      .filter(_.length > 0 )
      .map(Signal(_))

  override def part1(input: Stream[IO, String]): IO[String] =
    signals(input)
      .map(_.identifiableOutputs.length)
      .reduce(_ + _)
      .compile.toList.map(_.head.toString)


  override def part2(input: Stream[IO, String]): IO[String] =
    signals(input).map(s => s.outputValue).reduce(_ + _)
      .compile.toList.map(_.head.toString)
}
