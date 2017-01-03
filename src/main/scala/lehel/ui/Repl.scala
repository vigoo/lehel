package lehel.ui

import lehel.dsl.ImmediateExecutor
import lehel.model.{ExecutionError, ExitRequest, SystemState}
import lehel.parser.Parser

import scala.annotation.tailrec

trait Repl {

  def run(): Unit = {
    readNextLine(SystemState.initial)
  }

  protected def getNextLine(state: SystemState): Option[String]

  protected def showResult[A](result: A): Unit

  protected def showError(error: ExecutionError): Unit

  protected def close(): Unit

  @tailrec
  private def readNextLine(state: SystemState): Unit = {
    getNextLine(state) match {
      case Some(line) =>
        Parser.parseCommand(line) match {
          case Left(error) =>
            println(error)
            readNextLine(state)
          case Right(program) =>
            program.foldMap(ImmediateExecutor.executor).value.run(state).value match {
              case (newState, Left(ExitRequest)) =>
                close()
              case (newState, Left(error)) =>
                showError(error)
                readNextLine(newState)
              case (newState, Right(result)) =>
                showResult(result)
                readNextLine(newState)
            }
        }
      case None =>
    }
  }
}
