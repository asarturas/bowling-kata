package com.spikerlabs.bowling

import com.spikerlabs.bowling.Frame
import com.spikerlabs.bowling.Frame.{Strike, StrikeWithIncompleteBonus}
import org.scalatest.{FlatSpec, Matchers}

class FrameSpec extends FlatSpec with Matchers {

  it should "have zero score initially" in {
    Frame.Ordinary().score shouldBe 0
  }

  it should "score the first roll" in {
    Frame.roll(1).score shouldBe 1
  }

  it should "score the second roll" in {
    Frame.roll(1).roll(2).score shouldBe 3
  }

  it should "be a strike when all 10 pins are knocked down by first roll" in {
    val strike = Frame.roll(10)
    strike.isStrike shouldBe true
    strike.score shouldBe 10
  }

  it should "be a spare when all 10 pins are knocked down by two rolls" in {
    val spare = Frame.roll(1).roll(9)
    spare.isSpare shouldBe true
    spare.score shouldBe 10
  }

  behavior of "Ordinary frame overflow"

  it should "open next ordinary frame from full ordinary frame" in {
    Frame.Ordinary(Some(1), Some(2)).roll(3) shouldBe
      Frame.Ordinary(Some(3), None,
        Frame.Ordinary(Some(1), Some(2))
      )
  }

  behavior of "Spare transition and bonus calculation"

  it should "transition spare without bonus -> spare" in {
    Frame.roll(1).roll(9).roll(2) shouldBe Frame.Ordinary(Some(2), None, Frame.Spare(2, 1, 9))
  }

  behavior of "Strike transition and bonus calculation"

  it should "transition strike without bonus -> strike with partial bonus" in {
    Frame.roll(10).roll(1) shouldBe Frame.Ordinary(Some(1), None, StrikeWithIncompleteBonus(1))
  }

  it should "transition strike with partial bonus -> strike" in {
    Frame.roll(10).roll(1).roll(2) shouldBe Frame.Ordinary(Some(1), Some(2), Frame.Strike(3))
  }

  it should "transition of previous strike with partial bonus -> strike" in {
    Frame.roll(10).roll(10).roll(1) shouldBe Frame.Ordinary(Some(1), None, StrikeWithIncompleteBonus(1, Strike(11)))
  }

  behavior of "Score calculation"

  it should "add scores of ordinary frames" in {
    Frame.roll(1).roll(2).roll(3).score shouldBe 6
  }

  it should "double score of first roll after a spare" in {
    Frame.roll(1).roll(9).roll(3).roll(4).score shouldBe 20 // (1+9+3) + 3 + 4
  }

  it should "double the first and second rolls after a strike" in {
    Frame.roll(10).roll(2).roll(3).roll(4).score shouldBe 24 // (10+2+3) + 2 + 3 + 4
  }

  it should "add up scores of two consecutive spares" in {
    Frame.roll(1).roll(9).roll(1).roll(9).roll(1).roll(2).score shouldBe 25 // (1+9+1) + (1+9+1) + 1 + 2
  }

  it should "add up scores of three consecutive strikes" in {
    Frame.roll(10).roll(10).roll(10).roll(1).roll(2).roll(3).score shouldBe 70 // (10+10+10) + (10+10+1) + (10+1+2) + 1 + 2 + 3
  }

  it should "calculate score of a spare, followed by a strike" in {
    Frame.roll(1).roll(9).roll(10).roll(1).roll(2).roll(3).score shouldBe 39 // (1+9+10) + (10+1+2) + 1 + 2 + 3
  }

  it should "calculate the gutted game" in {
    Frame
      .roll(0).roll(0)
      .roll(0).roll(0)
      .roll(0).roll(0)
      .roll(0).roll(0)
      .roll(0).roll(0)
      .roll(0).roll(0)
      .roll(0).roll(0)
      .roll(0).roll(0)
      .roll(0).roll(0)
      .roll(0).roll(0).score shouldBe 0
  }

  it should "calculate the perfect game" in {
    Frame
      .roll(10)
      .roll(10)
      .roll(10)
      .roll(10)
      .roll(10)
      .roll(10)
      .roll(10)
      .roll(10)
      .roll(10)
      .roll(10).roll(10).score shouldBe 300
  }

  it should "calculate game with only 1s" in {
    Frame
      .roll(1).roll(1)
      .roll(1).roll(1)
      .roll(1).roll(1)
      .roll(1).roll(1)
      .roll(1).roll(1)
      .roll(1).roll(1)
      .roll(1).roll(1)
      .roll(1).roll(1)
      .roll(1).roll(1)
      .roll(1).roll(1).score shouldBe 20
  }

  it should "calculate game with only spares" in {
    Frame
      .roll(1).roll(9)
      .roll(1).roll(9)
      .roll(1).roll(9)
      .roll(1).roll(9)
      .roll(1).roll(9)
      .roll(1).roll(9)
      .roll(1).roll(9)
      .roll(1).roll(9)
      .roll(1).roll(9)
      .roll(1).roll(9).roll(1).score shouldBe 110
  }

  behavior of "Printing the frame"

  it should "print ordinary empty frame" in {
    Frame.Ordinary().toString shouldBe "[_ _] _ "
  }

  it should "print ordinary frame after one roll" in {
    Frame.Ordinary(Some(1)).toString shouldBe "[1 _] _ "
  }

  it should "print ordinary frame after two rolls" in {
    Frame.Ordinary(Some(1), Some(2)).toString shouldBe "[1 2] 3 "
  }

  it should "print spare without a bonus" in {
    Frame.SpareWithoutBonus(1, 9).toString shouldBe "[1 /] _ "
  }

  it should "print spare with a bonus" in {
    Frame.Spare(1, 1, 9).toString shouldBe "[1 /] 11 "
  }

}
