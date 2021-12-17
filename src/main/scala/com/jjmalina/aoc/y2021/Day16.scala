package com.jjmalina.aoc.y2021

import cats._
import cats.effect._
import cats.syntax.all._
import fs2._
import scala.math.BigInt
import com.jjmalina.aoc.AOCApp

object Day16 extends AOCApp(2021, 16) {

  val Hex2Bin = Map(
    '0' -> "0000",
    '1' -> "0001",
    '2' -> "0010",
    '3' -> "0011",
    '4' -> "0100",
    '5' -> "0101",
    '6' -> "0110",
    '7' -> "0111",
    '8' -> "1000",
    '9' -> "1001",
    'A' -> "1010",
    'B' -> "1011",
    'C' -> "1100",
    'D' -> "1101",
    'E' -> "1110",
    'F' -> "1111",
  )

  sealed trait Packet {
    def version: Int
    def typeId: Int
  }
  case class LiteralPacket(version: Int, typeId: Int, literal: BigInt) extends Packet
  case class OperatorPacket(version: Int, typeId: Int, lengthTypeId: Int) extends Packet

  def hex2bin(s: String): String = s.map(Hex2Bin(_)).mkString

  def parseFirstThreeAsInt(packetString: String): (Int, String) =
    (Integer.parseInt(packetString.take(3), 2), packetString.drop(3))

  def parsePacketVersionAndTypeId(packetString: String): (Int, Int, String) = {
    val (version, rest) = parseFirstThreeAsInt(packetString)
    val (typeId, remainder) = parseFirstThreeAsInt(rest)
    (version, typeId, remainder)
  }

  def parseLiteralPacketBits(s: String): (BigInt, String) = {
    def parseBitSequences(acc: List[String], rest: String): (List[String], String) ={
      if (rest.length < 5) {
        (acc, rest)
      } else {
        val bits = rest.take(5)
        val newAcc = acc :+ bits.tail
        if (bits.startsWith("1")) {
          parseBitSequences(newAcc, rest.drop(5))
        } else {
          (newAcc, rest.drop(5))
        }
      }
    }
    val (bits, rest) = parseBitSequences(List(), s)
    (BigInt(bits.mkString, 2), rest)
  }

  def parseLiteralPacket(version: Int, typeId: Int, rest: String): (LiteralPacket, String) = {
    val (literal, remaining) = parseLiteralPacketBits(rest)
    (LiteralPacket(version, typeId, literal), remaining)
  }

  def parseLengthTypeId(s: String): (String, String) = (s.head.toString, s.drop(1))

  def parseOperatorPacket(version: Int, typeId: Int, s: String): (OperatorPacket, String) = {
    val (lengthTypeId, rest) = parseLengthTypeId(s)
    if (lengthTypeId == "0") {
      val (subPacketsBitLength, remainder) = (rest.take(15), rest.drop(15))
      (
        OperatorPacket(version, typeId, lengthTypeId.toInt),
        remainder
      )
    } else {
      val (numberOfSubPackets, remainder) = (rest.take(11), rest.drop(11))
      (
        OperatorPacket(version, typeId, lengthTypeId.toInt),
        remainder
      )
    }

  }

  def parse(s: String): List[Packet] = {
    def loop(packets: List[Packet], s: String): List[Packet] = {
      if (s.length < 7) {
        packets
      } else {
        val (version, typeId, remainder) = parsePacketVersionAndTypeId(s)
        val (packet, rest) = if (typeId == 4) {
          parseLiteralPacket(version, typeId, remainder)
        } else {
          parseOperatorPacket(version, typeId, remainder)
        }
        loop(packets :+ packet, rest)
      }
    }
    loop(List(), s)
  }

  override def part1(input: Stream[IO, String]): IO[String] =
    input.through(text.lines).filter(_.length > 0).compile.toList.map(lines =>
      parse(lines.head).map(_.version).sum.toString
    )
  override def part2(input: Stream[IO, String]): IO[String] = ???
}
