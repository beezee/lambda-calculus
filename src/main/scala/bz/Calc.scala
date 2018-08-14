package bz

import scalaz.{Isomorphism => iso, Show, \/, -\/, \/-}
import iso.<=>
import scalaz.std.list._
import scalaz.syntax.either._

object calc extends App {
  sealed trait Exp
  object Exp {
    type AB = \/[applied, body]
    type PAB = \/[param, AB]
    type CPAB = \/[constant, PAB]
    type ExpCop = \/[function, CPAB]
    implicit val gen: (Exp <=> ExpCop) =
      iso.IsoSet((x: Exp) => x match {
        case f: function => f.left[CPAB]
        case c: constant => c.left[PAB].right[function]
        case p: param => p.left[AB].right[constant].right[function]
        case b: body => b.right[applied].right[param].right[constant].right[function]
        case a: applied => a.left[body].right[param].right[constant].right[function]
       }, (x: ExpCop) => x match {
        case -\/(f) => f
        case \/-(-\/(c)) => c
        case \/-(\/-(-\/(p))) => p
        case \/-(\/-(\/-(-\/(a)))) => a
        case \/-(\/-(\/-(\/-(b)))) => b
      })
  }

  case class function(params: Set[param], exp: Exp) extends Exp
  object function {
    implicit val gen: (function <=> (Set[param], Exp)) =
      iso.IsoSet((x: function) => (x.params, x.exp), (function.apply _).tupled)

    //implicit val show: Show[function] = Show.fromIso(gen)
  }

  case class constant(value: Symbol) extends Exp
  object constant {
    implicit val show: Show[constant] = Show.shows((p: constant) => p.value.toString)
  }

  case class param(value: Symbol) extends Exp
  object param {
    implicit val show: Show[param] = Show.shows((p: param) => p.value.toString)
  }

  case class applied(params: List[param]) extends Exp
  object applied {
    implicit val gen: (applied <=> List[param]) = iso.IsoSet(_.params, applied(_))
    implicit val show: Show[applied] = Show.fromIso(gen)
  }

  case class body(exp: List[Exp]) extends Exp

  val example =
    body(List(
      function(Set(param('x)), body(List(constant('x), constant('x)))),
      function(Set(param('y)), body(List(constant('y), constant('x)))),
      constant('z)))
}
