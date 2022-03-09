package ktedon.suicide

import scala.io.Source

@main def hello() =

  val file = Source.fromFile("testcode.lpp")

  val fileContents = IO.withResourcesAutoclose(file) { resource =>
    resource.getLines.mkString("\n")
  }

  // println(fileContents)

  TokenParsers.fileParts.parse(fileContents) match {
    case Left(i) => println(i)
    case Right(i) => println(i)
  }
