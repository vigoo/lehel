package lehel.dsl

import cats.free.Free
import cats.implicits._
import lehel.model.Processes.{ExitCode, ProcessSystem}
import lehel.model._

import scala.sys.process.ProcessBuilder

object Helpers {

  import Commands._

  def switch(): Free[Command, Side] =
    for {
      currentSide <- getCurrentSide()
      newSide = currentSide.otherSide
      _ <- setCurrentSide(newSide)
    } yield newSide

  def getPath(): Free[Command, Path] =
    for {
      currentSide <- getCurrentSide()
      currentPath <- getCurrentPath(currentSide)
    } yield currentPath

  def changePath(newPath: Path): Free[Command, Unit] =
    for {
      currentSide <- getCurrentSide()
      _ <- setCurrentPath(currentSide, newPath)
    } yield ()

  def changePath(rawNewPath: String): Free[Command, Unit] =
    for {
      currentSide <- getCurrentSide()
      currentPath <- getCurrentPath(currentSide)
      newPath = FileSystemPath.fromString(currentPath, rawNewPath)
      _ <- setCurrentPath(currentSide, newPath)
    } yield ()

  def executeAndWait(process: String, args: String*): Free[Command, ExitCode] =
    for {
      processSystem <- defineProcess(process, args : _*)
      runningProcess <- executeProcess(processSystem)
      resultCode = runningProcess.waitForExit()
    } yield resultCode

  def pipeProcesses(existingBuilder: ProcessSystem, processes: Seq[Either[String, (String, Seq[String])]]): Free[Command, ProcessSystem] =
    processes match {
      case Seq() => Free.pure(existingBuilder)
      case Left(code) +: tail =>
        for {
          next <- pipeCode(existingBuilder, code)
          last <- pipeProcesses(next, tail)
        } yield last
      case Right((process, args)) +: tail =>
        for {
          next <- pipeProcess(existingBuilder, process, args : _*)
          last <- pipeProcesses(next, tail)
        } yield last
    }
}
