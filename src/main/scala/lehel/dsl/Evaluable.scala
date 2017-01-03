package lehel.dsl

trait Evaluable[T] {
  def result(evalResult: Any): T
}

object Evaluable {
  implicit object EvaluableString extends Evaluable[String] {
    def result(evalResult: Any): String = evalResult.asInstanceOf[String]
  }
}
