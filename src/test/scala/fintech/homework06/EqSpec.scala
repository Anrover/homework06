package fintech.homework06
import org.scalatest.{FlatSpec, Matchers}
import fintech.homework06.EqSyntax._

class EqSpec extends FlatSpec with Matchers {
  "Eq" should "correct compare String values" in {
    "aaa" === "aaa" should be (true)
    "aaa" === "aab" should be (false)
  }

  it should "correct compare Int values" in {
    10 == 10 should be (true)
    10 == 20 should be (false)
  }

  it should "correct compare Map values" in {
    val map1 = Map("a" -> 1, "b" -> 2, "c" -> 3)
    val map1WithBiggerSize = Map("a" -> 1, "b" -> 2, "c" -> 3, "d" -> 4)
    val map2 = Map("b" -> 2, "c" -> 3, "a" -> 1)

    map1 ==== map2 should be (true)
    map1 ==== map1WithBiggerSize should be (false)
    Map("b" -> 2) ==== Map("b" -> 3) should be (false)
    Map.empty[Int, Int] ==== Map.empty[Int, Int] should be (true)
  }

  it should "correct compare Seq values" in {
    Seq(1, 2, 3) ==== Seq(1, 2, 3) should be (true)
    Seq(1, 2, 3) ==== Seq(1, 2, 3, 4) should be (false)
    Seq(1, 2, 3) ==== Seq(1, 2, 4) should be (false)
  }

  it should "correct compare Option values" in {
    Option("aa") ==== Option("aa") should be (true)
    Option("aa") ==== Option("bb") should be (false)
    Option.empty[Int] ==== Option.empty[Int] should be (true)
    Option.empty[Int] ==== Option(10) should be (false)
  }

  it should "correct compare values with complex types" in {
    Map(
      1 -> Seq(Option("a"), Option("b"), Option("c"), Option.empty[String]),
      2 -> Seq(Option("aa"), Option("bb"), Option("cc")),
      3 -> Seq(Option("aaa"), Option("bbb"), Option("ccc"), Option.empty[String]),
      4 -> Seq(Option("aaaa"), Option("bbbb"), Option("cccc"), Option.empty[String])
    ) ==== Map(
      4 -> Seq(Option("aaaa"), Option("bbbb"), Option("cccc"), Option.empty[String]),
      1 -> Seq(Option("a"), Option("b"), Option("c"), Option.empty[String]),
      2 -> Seq(Option("aa"), Option("bb"), Option("cc")),
      3 -> Seq(Option("aaa"), Option("bbb"), Option("ccc"), Option.empty[String])
    ) should be (true)

    Map(
      1 -> Seq(Option("a"), Option("b"), Option("c"), Option.empty[String]),
      2 -> Seq(Option("aa"), Option("bb"), Option("cc")),
      3 -> Seq(Option("aaa"), Option("bbb"), Option("ccc"), Option.empty[String]),
      4 -> Seq(Option("aaaa"), Option("bbbb"), Option("cccc"), Option.empty[String])
    ) ==== Map(
      4 -> Seq(Option("aaaa"), Option("bbbb"), Option("cccc"), Option.empty[String]),
      1 -> Seq(Option("a"), Option("b"), Option("c"), Option.empty[String]),
      2 -> Seq(Option("aa"), Option("bb"), Option("cc")),
      3 -> Seq(Option("aaa"), Option("bbb"), Option("ERRRRRRROR!!!"), Option.empty[String])
    ) should be (false)
  }
}
