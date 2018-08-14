package bz

import scala.language.higherKinds
import scalaz.{Cord, Isomorphism => iso, Show, \/, -\/, \/-}
import iso.<=>
import scalaz.std.list._
import scalaz.std.set._
import scalaz.std.tuple._
import scalaz.syntax.either._
import scalaz.syntax.show._

trait Decidable[F[_]] {
  def choose2[Z, A1, A2](a1: =>F[A1], a2: =>F[A2])(f: Z => (A1 \/ A2)): F[Z]

  def choose3[Z, A1, A2, A3](a1: =>F[A1], a2: =>F[A2], a3: =>F[A3])(
    f: Z => A1 \/ (A2 \/ A3)
  ): F[Z] = {
    val a23: F[A2 \/ A3] = choose2(a2, a3)(identity)
    choose2(a1, a23)(f)
  }
  def choose4[Z, A1, A2, A3, A4](a1: =>F[A1], a2: =>F[A2], a3: =>F[A3], a4: =>F[A4])(
    f: Z => A1 \/ (A2 \/ (A3 \/ A4))
  ): F[Z] = {
    val a234: F[A2 \/ (A3 \/ A4)] = choose3(a2, a3, a4)(identity)
    choose2(a1, a234)(f)
  }
  def choose5[Z, A1, A2, A3, A4, A5](a1: =>F[A1], a2: =>F[A2], a3: =>F[A3], a4: =>F[A4], a5: =>F[A5])(
    f: Z => A1 \/ (A2 \/ (A3 \/ (A4 \/ A5)))
  ): F[Z] = {
    val a2345: F[A2 \/ (A3 \/ (A4 \/ A5))] = choose4(a2, a3, a4, a5)(identity)
    choose2(a1, a2345)(f)
  }
}

object Decidable {
  implicit val showInstance: Decidable[Show] = new Decidable[Show] {
    def choose2[Z, A1, A2](a1: =>Show[A1], a2: =>Show[A2])(f: Z => (A1 \/ A2)): Show[Z] =
      new Show[Z] {
        def show(z: Z): Cord = f(z).fold(a1.show(_), a2.show(_))
      }
  }

  def apply[F[_]](implicit D: Decidable[F]) = D
}

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


    implicit val show: Show[Exp] = Decidable[Show].choose5(
      Show[function], Show[constant], Show[param], Show[applied], Show[body])(gen.to(_))
  }

  case class function(params: Set[param], body: body) extends Exp
  object function {
    implicit val gen: (function <=> (Set[param], body)) =
      iso.IsoSet((x: function) => (x.params, x.body), (function.apply _).tupled)

    implicit val show: Show[function] = Show.fromIso(gen)
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
  object body {
    implicit val gen: (body <=> List[Exp]) = iso.IsoSet(_.exp, body(_))
    implicit val show: Show[body] = Show.fromIso(gen)
  }

  val example =
    body(List(
      function(Set(param('x)), body(List(constant('x), constant('x)))),
      function(Set(param('y)), body(List(constant('y), constant('x)))),
      constant('z)))

  println(example.show)
}
