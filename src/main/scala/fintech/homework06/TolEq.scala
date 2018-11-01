package fintech.homework06

trait TolEq[A] {
  def equiv(lft: A, rgt: A, tolerance: A): Boolean
}

final case class Spread[T : TolEq](pivot: T, tolerance: T)

trait TolEqInstances {
  import TolEqSyntax._
  import fintech.homework02.ComplexNumber

  implicit def doubleInstance: TolEq[Double] =
    (lft: Double, rgt: Double, tolerance: Double) => (lft - rgt).abs <= tolerance

  implicit def complexNumInstance: TolEq[ComplexNumber] =
    (lft: ComplexNumber, rgt: ComplexNumber, tolerance: ComplexNumber) =>
      (lft.real ==== rgt.real -+ tolerance.real) && (lft.imag ==== rgt.imag -+ tolerance.imag)
}

object TolEqSyntax {
  implicit class EqOps[T : TolEq](self: T) {
    def -+(tolerance: T): Spread[T] = new Spread[T](self, tolerance)
    def ====(other: Spread[T]): Boolean = implicitly[TolEq[T]]
      .equiv(self, other.pivot, other.tolerance)
  }
}

object TolEq extends TolEqInstances
