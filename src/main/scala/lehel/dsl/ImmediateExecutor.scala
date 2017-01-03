package lehel.dsl

import better.files.File
import cats.data.{EitherT, State}
import cats.implicits._
import cats.{Id, ~>}
import lehel.dsl.Commands._
import lehel.dsl.Commands.Evaluate
import lehel.model.Processes.ProcessSpecs
import lehel.model._

import scala.sys.process._
import scala.util.{Failure, Success, Try}
import scala.reflect.runtime._
import scala.tools.reflect.ToolBox

object ImmediateExecutor {
  type ExecutorState[A] = State[SystemState, A]
  type ExecutorMonad[A] = EitherT[ExecutorState, ExecutionError, A]

  val executor = new (Command ~> ExecutorMonad) {
    val cm = universe.runtimeMirror(getClass.getClassLoader)
    val toolbox = cm.mkToolBox()

    override def apply[A](fa: Command[A]): ExecutorMonad[A] = fa match {
      case GetCurrentSide =>
        EitherT.right {
          for {
            systemState <- State.get[SystemState]
          } yield systemState.currentSide
        }

      case SetCurrentSide(newSide) =>
        EitherT.right {
          State.modify[SystemState](_.copy(currentSide = newSide))
        }

      case GetCurrentPath(side) =>
        EitherT.right {
          for {
            systemState <- State.get[SystemState]
          } yield systemState.currentSideState.currentPath
        }

      case SetCurrentPath(side, newPath) =>
        EitherT.right {
          State.modify[SystemState] { systemState =>
            systemState.currentSide match {
              case LeftSide => systemState.copy(leftState = systemState.leftState.copy(currentPath = newPath))
              case RightSide => systemState.copy(rightState = systemState.rightState.copy(currentPath = newPath))
            }
          }
        }

      case DefineProcess(process, args) =>
        for {
          physicalDirectory <- requirePhysicalCurrentDirectory()
        } yield Processes.singleProcess(physicalDirectory, process, args: _*)

      case PipeProcess(currentProcessSystem, process, args) =>
        for {
          physicalDirectory <- requirePhysicalCurrentDirectory()
          pipedProcess = ProcessSpecs(physicalDirectory, process, args: _*)
        } yield currentProcessSystem.pipeTo(pipedProcess)

      case PipeCode(currentProcessSystem, code) =>
        Try {
          val ast = toolbox.parse(code)
          toolbox.eval(ast).asInstanceOf[String => Any]
        } match {
          case Success(processFn) =>
            EitherT.right {
              State.pure(currentProcessSystem.pipeTo(
                LineProcessor.from(processFn.andThen(_.toString))))
            }

          case Failure(reason) =>
            EitherT.left {
              State.pure(FailedWith(reason))
            }
        }

      case ExecuteProcess(processSystem) =>
        processSystem.start() match {
          case Success(process) => EitherT.right { State.pure(process) }
          case Failure(reason) => EitherT.left { State.pure(FailedWith(reason)) }
        }

      case Evaluate(code, evaluable) =>
        Try {
          val ast = toolbox.parse(code)
          evaluable.result(toolbox.eval(ast))
        } match {
          case Success(value) => EitherT.right { State.pure(value) }
          case Failure(reason) => EitherT.left { State.pure(FailedWith(reason)) }
        }

      case Exit =>
        EitherT.left(State.pure(ExitRequest))
    }
  }

  private def requirePhysicalCurrentDirectory(): ExecutorMonad[File] = {
    EitherT {
      State.get[SystemState].map { systemState =>
        val currentDirectory = systemState.currentSideState.currentPath
        PathOperations.toPhysicalPath(currentDirectory) match {
          case Some(physicalDirectory) =>
            Right(physicalDirectory)
          case None =>
            Left(CannotExecuteProcessHere(currentDirectory))
        }
      }
    }
  }
}
