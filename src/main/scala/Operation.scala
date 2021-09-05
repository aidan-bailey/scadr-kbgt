package skbgen.logic

import skbgen.config._

sealed trait Operation extends Enumeration

/** Binary operator enums and maps. */
object BinOp extends Operation {

  var notation: NotationOption.Value = NotationOption.Tweety

  /** Binary operator enums. */
  val And, Or, Implies, Iff = Value

  /** Maps to symbolic representation. */
  def getNotation(op: BinOp.Value): String =
    notation match {
      case NotationOption.Tweety =>
        op match {
          case And     => "^^"
          case Or      => "||"
          case Implies => "=>"
          case Iff     => "<=>"
        }
      case NotationOption.Formal =>
        op match {
          case And     => "∧"
          case Or      => "∨"
          case Implies => "→"
          case Iff     => "↔"
        }
      case NotationOption.Simple =>
        op match {
          case And     => "&"
          case Or      => "|"
          case Implies => ">"
          case Iff     => "="
        }
      case NotationOption.Latex =>
        op match {
          case And     => "\\land "
          case Or      => "\\vee  "
          case Implies => "\\rightarrow "
          case Iff     => "\\leftrightarrow "
        }
    }

  /** Maps to function. */
  val functionMap: Map[BinOp.Value, (Boolean, Boolean) => Boolean] =
    Map(
      And -> ((p: Boolean, q: Boolean) => p && q),
      Or -> ((p: Boolean, q: Boolean) => p || q),
      Implies -> ((p: Boolean, q: Boolean) => p <= q),
      Iff -> ((p: Boolean, q: Boolean) => p == q)
    )
}

/** Unary operator enums and maps. */
object UnOp extends Operation {

  var notation: NotationOption.Value = NotationOption.Tweety

  /** Unary operator enums. */
  val Not = Value

  /** Maps to symbolic representation. */
  def getNotation(op: UnOp.Value): String =
    notation match {
      case NotationOption.Tweety =>
        op match {
          case Not => "!"
        }
      case NotationOption.Formal =>
        op match {
          case Not => "¬"
        }
      case NotationOption.Simple =>
        op match {
          case Not => "~"
        }
      case NotationOption.Latex =>
        op match {
          case Not => "\\neg "
        }
    }

  /** Maps to function. */
  val functionMap: Map[UnOp.Value, Boolean => Boolean] = Map(
    Not -> ((p: Boolean) => !p)
  )

}
