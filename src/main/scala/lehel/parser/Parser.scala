package lehel.parser

import cats.free.Free
import lehel.dsl.Commands._
import lehel.dsl.Evaluable._
import lehel.dsl.Helpers._

object Parser {

  sealed trait ParseFailure

  case class CouldNotParseInput(input: String) extends ParseFailure

  def parseCommand(line: String): Either[ParseFailure, Free[Command, _]] = {
    // TODO: real, extensible parser

    if (line.startsWith("cd ")) {
      val rawNewPath = line.substring(3)
      Right(changePath(rawNewPath))
    } else if (line.startsWith(">")) {
      val pipedParts = line.drop(1).trim.split('|')

      def processPipeSegment(segment: String): Either[String, (String, Seq[String])] = {
        if (segment.startsWith("$ ")) {
          Left(segment.drop(2))
        } else {
          val parts = segment.trim.split(' ')
          Right(parts.head, parts.tail.toSeq)
        }
      }

      val commands = for (segment <- pipedParts)
        yield processPipeSegment(segment)

      Right {
        val Right((firstProcess, firstArgs)) = commands.head

        for {
          firstBuilder <- defineProcess(firstProcess, firstArgs: _*)
          finalBuilder <- pipeProcesses(firstBuilder, commands.tail)
          process <- executeProcess(finalBuilder)
          resultCode = process.waitForExit()
        } yield resultCode
      }
    } else if (line.startsWith("$ ")) {
      val code = line.drop(1).trim
      Right(evaluate(code)(EvaluableString))
    } else {
      line match {
        case "switch" =>
          Right(switch())
        case "pwd" =>
          Right(getPath())
        case "exit" =>
          Right(exit())
        case _ =>
          Left(CouldNotParseInput(line))
      }
    }
  }
}
