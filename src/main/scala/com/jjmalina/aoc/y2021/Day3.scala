package com.jjmalina.aoc.y2021

import cats._
import cats.effect._
import cats.syntax.all._
import fs2._
import com.jjmalina.aoc.AOCApp
import cats.instances.boolean

object Day03 extends AOCApp(2021, 3) {

  case class BitCounter(one: Int = 0, zero: Int = 0)
  object BitCounter {
    def getCountersFromString(str: String): IndexedSeq[BitCounter] =
      str.map {
        case '1' => BitCounter(1, 0)
        case '0' => BitCounter(0, 1)
      }
  }

  implicit val bitCounterMonoid: Monoid[BitCounter] = new Monoid[BitCounter] {
    def empty: BitCounter = BitCounter(0, 0)
    def combine(a: BitCounter, b: BitCounter): BitCounter =
      BitCounter(a.one + b.one, a.zero + b.zero)
  }

  implicit val bitCountersMonoid: Monoid[IndexedSeq[BitCounter]] = new Monoid[IndexedSeq[BitCounter]] {
    def empty: IndexedSeq[BitCounter] = IndexedSeq()
    def combine(a: IndexedSeq[BitCounter], b: IndexedSeq[BitCounter]): IndexedSeq[BitCounter] = {
      if (a.length == 0) {
        b
      } else if (b.length == 0) {
        a
      } else {
        a.zip(b).map {
          case (c1, c2) => c1.combine(c2)
        }
      }
    }
  }

  def bits(input: Stream[IO, String]): Stream[IO, String] =
    input
      .through(text.lines)
      .filter(_.length > 0)

  def bitcounts(input: Stream[IO, String]): Stream[IO, IndexedSeq[BitCounter]] = {
    bits(input)
      .map(BitCounter.getCountersFromString)
      .foldMonoid
  }

  override def part1(input: Stream[IO, String]): IO[String] = {
    bitcounts(input)
      .map(counters => {
        val (gammaBin, epsilonBin): Tuple2[String, String] = counters.foldLeft(("", "")) {
          case ((gamma, epsilon), BitCounter(oneCount, zeroCount)) => {
            (
              gamma + (if (oneCount > zeroCount) "1" else "0"),
              epsilon + (if (oneCount > zeroCount) "0" else "1"),
            )
          }
        }
        val gamma = Integer.parseInt(gammaBin, 2)
        val epsilon = Integer.parseInt(epsilonBin, 2)
        gamma * epsilon
      })
      .compile.toList.map(_.head.toString)
  }

  def findRating(t: String, numbers: List[String], filterFn: (Char, BitCounter) => Boolean): List[String] = {
    (0 until numbers.head.length).toList.foldLeft(numbers)((currentNumbers, bitPosition) => {
      if (currentNumbers.length == 1) {
        currentNumbers
      } else {
        val counters = Monoid.combineAll(currentNumbers.map(BitCounter.getCountersFromString))
        val bitPositionCounter = counters(bitPosition)
        currentNumbers.filter(number => filterFn(number(bitPosition), bitPositionCounter))
      }
    })
  }

  override def part2(input: Stream[IO, String]): IO[String] = {
    bits(input).compile.toList.map(bitList => {
      val o2GenRating = findRating("o2", bitList, (bit, counter) => {
        (bit == '1' && counter.one >= counter.zero) || (bit == '0' && counter.zero > counter.one)
      }).head

      val co2ScrubRating = findRating("c02", bitList, (bit, counter) => {
        (bit == '1' && counter.one < counter.zero) || (bit == '0' && counter.zero <= counter.one)
      }).head
      (Integer.parseInt(o2GenRating, 2) * Integer.parseInt(co2ScrubRating, 2)).toString
    })
  }
}
