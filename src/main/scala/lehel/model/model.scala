package lehel.model

import better.files.Cmds._
import better.files.File

sealed trait Side {
  def otherSide: Side
}

case object LeftSide extends Side {
  override def otherSide: Side = RightSide
}

case object RightSide extends Side {
  override def otherSide: Side = LeftSide
}

case class SideState(currentPath: Path)

case class SystemState(currentSide: Side,
                       leftState: SideState,
                       rightState: SideState) {
  def currentSideState: SideState =
    currentSide match {
      case LeftSide => leftState
      case RightSide => rightState
    }
}

object SystemState {
  val initial: SystemState = SystemState(
    currentSide = LeftSide,
    leftState = SideState(currentPath = FileSystemPath(File.home)),
    rightState = SideState(currentPath = FileSystemPath(File.home))
  )
}

sealed trait ExecutionError

case class FailedWith(reason: Throwable) extends ExecutionError

case object ExitRequest extends ExecutionError

case class CannotExecuteProcessHere(path: Path) extends ExecutionError


trait Path

case class FileSystemPath(path: File) extends Path

object FileSystemPath {
  def fromString(s: String): FileSystemPath = {
    FileSystemPath(File.root / s)
  }

  def fromString(currentPath: Path, s: String) = {
    currentPath match {
      case FileSystemPath(p) =>
        FileSystemPath(p / s)
    }
  }
}

object PathOperations {
  def toPhysicalPath(path: Path): Option[File] = {
    path match {
      case FileSystemPath(p) => Some(p)
    }
  }
}