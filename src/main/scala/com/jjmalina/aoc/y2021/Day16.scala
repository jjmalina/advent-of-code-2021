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
  case class LiteralPacket(
    version: Int,
    typeId: Int, literal: BigInt
  ) extends Packet
  case class OperatorPacket(
    version: Int,
    typeId: Int,
    lengthTypeId: Int,
    subpackets: List[Packet]
  ) extends Packet

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
      val nBits = Integer.parseInt(subPacketsBitLength, 2)
      val (subpacketBits, remainingBits) = (remainder.take(nBits), remainder.drop(nBits))
      val (subpackets, _) = parse(subpacketBits)
      val operatorPacket = OperatorPacket(
        version, typeId, lengthTypeId.toInt, subpackets
      )
      (
        operatorPacket,
        remainingBits
      )
    } else {
      val (numberOfSubPacketsBits, remainder) = (rest.take(11), rest.drop(11))
      val subpacketsCount = Integer.parseInt(numberOfSubPacketsBits, 2)
      val (subpackets, remainingBits) = parse(remainder, Some(subpacketsCount))
      (
        OperatorPacket(version, typeId, lengthTypeId.toInt, subpackets),
        remainingBits
      )
    }
  }

  def parse(s: String, nPackets: Option[Int] = None): (List[Packet], String) = {
    def loop(packets: List[Packet], s: String): (List[Packet], String) = {
      if (s.length < 7) {
        (packets, s)
      } else if (nPackets.isDefined && nPackets.map(_ == packets.length).getOrElse(false)) {
        (packets, s)
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

  def getVersions(p: Packet): List[Int] = p match {
    case LiteralPacket(version, typeId, literal) => List(version)
    case OperatorPacket(version, typeId, lengthTypeId, subpackets) => {
      List(version) ++ subpackets.flatMap(getVersions(_))
    }
  }

  def getVersionSum(packets: List[Packet]): Int =
    packets.flatMap(getVersions(_)).sum

  def evaluatePacket(p: Packet): BigInt = p match {
    case LiteralPacket(version, typeId, literal) => literal
    case OperatorPacket(version, typeId, lengthTypeId, subpackets) => {
      val values = evaluate(subpackets)
      typeId match {
        case 0 => values.sum
        case 1 => values.reduce(_ * _)
        case 2 => values.min
        case 3 => values.max
        case 5 => if (values(0) > values(1)) 1 else 0
        case 6 => if (values(0) < values(1)) 1 else 0
        case 7 => if (values(0) == values(1)) 1 else 0
        case _ => throw new Exception("undefined type id")
      }
    }
  }

  def evaluate(packets: List[Packet]): List[BigInt] = packets.map(evaluatePacket)

  override def part1(input: Stream[IO, String]): IO[String] =
    input.through(text.lines).filter(_.length > 0).compile.toList.map(lines =>
      getVersionSum(parse(lines.head)._1).toString
    )
  override def part2(input: Stream[IO, String]): IO[String] =
    input.through(text.lines).filter(_.length > 0).compile.toList.map(lines =>
      evaluate(parse(lines.head)._1).toString
    )
}
