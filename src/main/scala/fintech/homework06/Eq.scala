package fintech.homework06

/*
Реализовать тайп класс Eq[A] и синтаксис '===', деривацию для Map Seq Option
Опционально - разработать === для комплексных чисел с возможностью указать точность
*/

trait Eq[A] {
  def equiv(lft: A, rgt: A): Boolean
}

trait EqInstances {
  implicit def intInstance: Eq[Int] = (lft: Int, rgt: Int) => lft == rgt

  implicit def stringInstance: Eq[String] = (lft: String, rgt: String) => lft == rgt

  implicit def mapInstance[K: Eq, V: Eq]: Eq[Map[K, V]] = (lft: Map[K, V], rgt: Map[K, V]) => {
    val eqK = implicitly[Eq[K]]
    val eqV = implicitly[Eq[V]]
    lft.size == rgt.size && lft.forall {
      case (k1, v1) => rgt.foldLeft(false) {
        case (acc, (k2, v2)) => acc || (eqK.equiv(k1, k2) && eqV.equiv(v1, v2))
      }
    }
  }

  implicit def optionInstance[T: Eq]: Eq[Option[T]] = (lft: Option[T], rgt: Option[T]) =>
    (lft, rgt) match {
    case (Some(v1), Some(v2)) => implicitly[Eq[T]].equiv(v1, v2)
    case (None, None) => true
    case _ => false
  }

  implicit def seqInstance[T: Eq]: Eq[Seq[T]] = (lft: Seq[T], rgt: Seq[T]) =>
    lft.size == rgt.size && lft.zip(rgt).forall(p => implicitly[Eq[T]].equiv(p._1, p._2))
}

object EqSyntax {
  implicit class EqOps[T : Eq](self: T) {
    def ====(other: T): Boolean = implicitly[Eq[T]].equiv(self, other)
  }
}

object Eq extends EqInstances
