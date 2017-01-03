package lehel.ui.console

import jline.console.ConsoleReader
import lehel.model.{ExecutionError, LeftSide, RightSide, SystemState}
import lehel.ui.Repl
import org.fusesource.jansi.Ansi.Color._
import org.fusesource.jansi.Ansi._

class ConsoleRepl extends Repl {
  private val reader = new ConsoleReader()
  reader.setExpandEvents(false)

  override def getNextLine(state: SystemState): Option[String] = {
    reader.setPrompt(getPromptFor(state))
    Option(reader.readLine())
  }

  override def close(): Unit = {
    println("Goodbye.")
  }

  override def showError(error: ExecutionError): Unit = {
    println(ansi().fgBright(RED).a(error.toString).reset())
  }

  override def showResult[A](result: A): Unit = {
    println(ansi().fgBright(GREEN).a(result.toString).reset())
  }

  private def getPromptFor(state: SystemState): String = {
    state.currentSide match {
      case LeftSide =>
        ansi().fgBright(BLUE).a(state.leftState.currentPath).reset().a("> ").toString
      case RightSide =>
        ansi().fgBright(RED).a(state.rightState.currentPath).reset().a("> ").toString
    }
  }
}
