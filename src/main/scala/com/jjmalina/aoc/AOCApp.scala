/*
borrowed from https://github.com/s5bug/aoc
*/
package com.jjmalina.aoc

import cats._
import cats.effect._
import cats.effect.std._
import fs2._
import org.http4s._
import org.http4s.implicits._
import org.http4s.jdkhttpclient.JdkHttpClient

abstract class AOCApp(year: Int, day: Int) extends IOApp {

  def part1(input: Stream[IO, String]): IO[String]
  def part2(input: Stream[IO, String]): IO[String]

  override def run(args: List[String]): IO[ExitCode] = {
    val aocToken = IO(sys.env("AOC_SESSION_COOKIE"))

    JdkHttpClient.simple[IO].use { client =>
      aocToken.flatMap { sessionCookie =>
        val req = Request[IO](
          uri = uri"https://adventofcode.com" / year.toString / "day" / day.toString / "input"
        ).addCookie("session", sessionCookie)
        val body = client.stream(req).flatMap(_.body).through(text.utf8.decode)

        part1(body).flatMap(Console[IO].println(_)) >>
          part2(body).flatMap(Console[IO].println(_))
      }
    }.as(ExitCode.Success)
  }

}
