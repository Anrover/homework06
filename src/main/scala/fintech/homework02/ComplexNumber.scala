package fintech.homework02

class ComplexNumber(val real: Double, val imag: Double) {
  def *(that: ComplexNumber): ComplexNumber =
    new ComplexNumber(
      this.real * that.real - this.imag * that.imag,
      this.real * that.imag + this.imag * that.real
    )

  def +(that: ComplexNumber): ComplexNumber =
    new ComplexNumber(this.real + that.real, this.imag + that.imag)

  def ~(power: Int): ComplexNumber = {
    val expModuleComplex = Math.pow(calcModuleComplex(this), power)
    val argumentComplex = calcArgumentComplex(this)
    new ComplexNumber(
      expModuleComplex * Math.cos(power * argumentComplex),
      expModuleComplex * Math.sin(power * argumentComplex)
    )
  }

  private def calcModuleComplex(number: ComplexNumber): Double =
    Math.sqrt(number.real * number.real + number.imag * number.imag)

  private def calcArgumentComplex(number: ComplexNumber): Double =
    if (number.real > 0)
      Math.atan(number.imag / number.real)
    else if(number.real < 0)
      Math.atan(number.imag / number.real) + Math.PI
    else
      Math.signum(number.imag) * Math.PI / 2

  override def equals(obj: scala.Any): Boolean = {
    obj match {
      case that: ComplexNumber => that.real == this.real && that.imag == this.imag
      case _ => false
    }
  }

  override def hashCode(): Int = (real, imag).hashCode()

  override def toString: String = s"Complex($real, ${imag}i)"
}
