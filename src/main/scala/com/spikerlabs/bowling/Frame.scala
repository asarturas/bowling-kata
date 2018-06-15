package com.spikerlabs.bowling

trait Frame {
  def previousFrame: Frame
  def score: Int
  def roll(score: Int): Frame
  def isStrike: Boolean = false
  def isSpare: Boolean = false
  val lastFrame = 10
  private[Frame] def frameNumber(acc: Int = 1, interruptCounter: Int = lastFrame * 10): Int =
    if (interruptCounter == 0) acc
    else previousFrame match {
      case Frame.Nil => acc
      case _ => previousFrame.frameNumber(acc + 1, interruptCounter - 1)
    }
}

object Frame {

  def roll(rolled: Int): Frame = Frame.Ordinary(previousFrame = Nil).roll(rolled)

  case class Ordinary(
    firstRoll: Option[Int] = None,
    secondRoll: Option[Int] = None,
    previousFrame: Frame = Nil
  ) extends Frame {

    override def score: Int = firstRoll.getOrElse(0) + secondRoll.getOrElse(0) + previousFrame.score

    override def roll(score: Int): Frame = {
      val newPrevious = previousFrame match {
        case StrikeWithIncompleteBonus(bonusSoFar, previousPreviousFrame) => Strike(bonusSoFar + score, previousPreviousFrame)
        case StrikeWithoutBonus(previousPreviousFrame) => StrikeWithIncompleteBonus(score, previousPreviousFrame)
        case SpareWithoutBonus(first, second, previousPreviousFrame) => Spare(score, first, second, previousPreviousFrame)
        case _ => previousFrame
      }
      // first roll
      if (firstRoll.isEmpty) {
        if (score == 10) StrikeWithoutBonus(newPrevious)
        else this.copy(firstRoll = Some(score), previousFrame = newPrevious)
      // second roll
      } else if (secondRoll.isEmpty) {
        if (firstRoll.map(_ + score).contains(10)) SpareWithoutBonus(firstRoll.get, score, newPrevious)
        else this.copy(secondRoll = Some(score), previousFrame = newPrevious)
      // overflow
      } else {
        Frame.Ordinary(previousFrame = this).roll(score)
      }
    }

    override def toString: String =
      if (firstRoll.isEmpty) "[_ _] _ "
      else if (secondRoll.isEmpty) s"[${firstRoll.get} _] _ "
      else s"[${firstRoll.get} ${secondRoll.get}] $score "
  }

  case class StrikeWithoutBonus(previousFrame: Frame = Nil) extends Frame {
    override val isStrike = true
    override def score: Int = 10 + previousFrame.score
    override def roll(rolled: Int): Frame = previousFrame match {
      case StrikeWithIncompleteBonus(bonusSoFar, previousPreviousFrame) =>
        Frame.Ordinary(
          previousFrame = this.copy(previousFrame = Strike(bonusSoFar + rolled, previousPreviousFrame))
        ).roll(rolled)
      case _ =>
        Frame.Ordinary(previousFrame = this).roll(rolled)
    }
  }

  case class StrikeWithIncompleteBonus(bonusSoFar: Int, previousFrame: Frame = Nil) extends Frame {
    override val isStrike = true
    override def score: Int = 10 + bonusSoFar + previousFrame.score
    override def roll(score: Int): Frame = throw new Exception("illegal move")
  }

  case class Strike(bonus: Int, previousFrame: Frame = Nil) extends Frame {
    override val isStrike = true
    override def score: Int = 10 + bonus + previousFrame.score
    override def roll(score: Int): Frame = throw new Exception("illegal move")
  }

  case class Spare(bonus: Int, firstRoll: Int, secondRoll: Int, previousFrame: Frame = Nil) extends Frame {
    override val isSpare = true
    override def score: Int = bonus + firstRoll + secondRoll + previousFrame.score
    override def roll(score: Int): Frame = throw new Exception("illegal move")
    override def toString: String = s"[$firstRoll /] $score "
  }

  case class SpareWithoutBonus(firstRoll: Int, secondRoll: Int, previousFrame: Frame = Nil) extends Frame {
    override val isSpare = true
    override def score: Int = firstRoll + secondRoll + previousFrame.score
    override def roll(score: Int): Frame = {
      if (frameNumber() == lastFrame) Spare(score, firstRoll, secondRoll, previousFrame)
      else Frame.Ordinary(previousFrame = Spare(score, firstRoll, secondRoll, previousFrame)).roll(score)
    }
    override def toString: String = s"[$firstRoll /] _ "
  }

  case object Nil extends Frame {
    val previousFrame: Frame = Nil
    val score: Int = 0
    def roll(score: Int): Frame = throw new Exception("illegal move")
  }

}
