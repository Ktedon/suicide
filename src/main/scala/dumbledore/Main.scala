package ktedon.dumbledore

import scala.io.Source

@main def hello() =

  val file = Source.fromFile("testcode.lpp")

  val fileContents = IO.withResourcesAutoclose(file) { resource =>
    resource.getLines.mkString("\n")
  }

  TokenParsers.namespace.parse(fileContents) match {
    case Left(i) => println(i.failedAtOffset)
    case Right(i) => println(i)
  }
