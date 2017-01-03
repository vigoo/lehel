package lehel.model

import lehel.model.Processes.ExitCode

case class LineProcessor(process: Either[ExitCode, String] => Either[ExitCode, String])

object LineProcessor {
  def from(fn: String => String): LineProcessor =
    LineProcessor {
      case Left(exitCode) => Left(exitCode)
      case Right(line) => Right(fn(line))
    }
}
