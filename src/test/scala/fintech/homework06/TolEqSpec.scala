package fintech.homework06
import org.scalatest.{FlatSpec, Matchers}
import TolEqSyntax._
import fintech.homework02.ComplexNumber

class TolEqSpec extends FlatSpec with Matchers {
  "TolEq" should "correct compare Double values with tolerance" in {
    123.1111111111112 ==== 123.1111111111155 -+ 0.0000000001 should be (true)
    123.1111111111112 ==== 123.1111111111155 -+ 25 should be (true)
    123.12 ==== 123.155 -+ 0.0000001 should be (false)
    123.12 ==== 123.155 -+ 0 should be (false)
    123.12 ==== 123.12 -+ 0.0 should be (true)
  }

  it should "correct compare ComplexNumber values with tolerance" in {
    val num1 = new ComplexNumber(12.111222333, 10.111222333)
    val num2 = new ComplexNumber(12.111222333, 10.111222334)
    val num3 = new ComplexNumber(12.111222334, 10.115555555)
    val precision = new ComplexNumber(0.00000001, 0.0000001)

    num1 ==== num2 -+ precision should be (true)
    num1 ==== num3 -+ precision should be (false)
  }
}
