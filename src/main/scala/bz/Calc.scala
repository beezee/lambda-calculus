package bz

import scala.language.higherKinds
import scalaz.{Cord, Isomorphism => iso, Show, \/, -\/, \/-}
import iso.<=>
import iotaz.{Cop, TNil}
import iotaz.TList.::
import scala.annotation.switch
import scalaz.std.list._
import scalaz.std.tuple._
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

object Cops {

  def to1[A1](c: Cop[A1 :: TNil]): A1 = c.value.asInstanceOf[A1]
  def to2[A1, A2](c: Cop[A1 :: A2 :: TNil]): A1 \/ A2 =
    (c.index: @switch) match {
      case 0 => -\/(c.value.asInstanceOf[A1])
      case 1 => \/-(c.value.asInstanceOf[A2])
    }
  def to3[A1, A2, A3](c: Cop[A1 :: A2 :: A3 :: TNil]): A1 \/ (A2 \/ A3) =
    (c.index: @switch) match {
      case 0 => -\/(c.value.asInstanceOf[A1])
      case 1 => \/-(-\/(c.value.asInstanceOf[A2]))
      case 2 => \/-(\/-(c.value.asInstanceOf[A3]))
    }
  def to4[A1, A2, A3, A4](
    c: Cop[A1 :: A2 :: A3 :: A4 :: TNil]
  ): A1 \/ (A2 \/ (A3 \/ A4)) = (c.index: @switch) match {
    case 0 => -\/(c.value.asInstanceOf[A1])
    case 1 => \/-(-\/(c.value.asInstanceOf[A2]))
    case 2 => \/-(\/-(-\/(c.value.asInstanceOf[A3])))
    case 3 => \/-(\/-(\/-(c.value.asInstanceOf[A4])))
  }
  def to5[A1, A2, A3, A4, A5](
    c: Cop[A1 :: A2 :: A3 :: A4 :: A5 :: TNil]
  ): A1 \/ (A2 \/ (A3 \/ (A4 \/ A5))) = (c.index: @switch) match {
    case 0 => -\/(c.value.asInstanceOf[A1])
    case 1 => \/-(-\/(c.value.asInstanceOf[A2]))
    case 2 => \/-(\/-(-\/(c.value.asInstanceOf[A3])))
    case 3 => \/-(\/-(\/-(-\/(c.value.asInstanceOf[A4]))))
    case 4 => \/-(\/-(\/-(\/-(c.value.asInstanceOf[A5]))))
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
    type ExpCop = Cop[function :: constant :: param :: body :: TNil]
    implicit val gen = Cop.gen[Exp, ExpCop#L]

    implicit val show: Show[Exp] = Decidable[Show].choose4(
      Show[function], Show[constant], Show[param], Show[body])(x => Cops.to4(gen.to(x)))
  }

  case class function(params: List[param], body: body) extends Exp
  object function {
    implicit val gen: (function <=> (List[param], body)) =
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

  case class body(exp: List[Exp]) extends Exp
  object body {
    implicit val gen: (body <=> List[Exp]) = iso.IsoSet(_.exp, body(_))
    implicit val show: Show[body] = Show.fromIso(gen)
  }

  val example =
    body(List(
      function(List(param('x)), body(List(constant('x), constant('x)))),
      function(List(param('y)), body(List(constant('y), constant('x)))),
      constant('z)))

  def applyFn(fn: function, c: constant) = {
    // was param passed? val upd = fn.
    // if so, return functon or body redefined
    // with matched param substituted by c
    ()
  }

  /*def reduce(exp: Exp) = exp match {
    case function(params, body) =>
    case body(exps) =>
    case param(s) =>
    case constant(s) => */

  println(example.show)
}
