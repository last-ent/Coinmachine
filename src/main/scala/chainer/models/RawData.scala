package chainer.models

import cats.Show
import cats.implicits._

sealed trait RawData {
  def toResponse: Response = Response(toLogMessage.value)

  def toLogMessage: LogMessage
}

sealed trait RawDataSuccess extends RawData

sealed trait RawDataErr extends RawData

case class NoValidInts(invalidInts: List[String]) extends RawDataErr {

  override def toLogMessage: LogMessage = LogMessage(this.show)
}

case class NoValuesAboveMinimum(
    invalidInts:     List[String],
    lessThanMinimum: List[String]
) extends RawDataErr {

  override def toLogMessage: LogMessage = LogMessage(this.show)
}

case class ValidIntsFound(
    invalidInts: List[String],
    validInts:   List[String]
) extends RawDataSuccess {

  override def toLogMessage: LogMessage = LogMessage(this.show)
}

case class IntsAboveMinimum(
    invalidInts:     List[String],
    lessThanMinimum: List[String],
    aboveMinimum:    List[String]
) extends RawDataSuccess {

  override def toLogMessage: LogMessage = LogMessage(this.show)
}

case class IntsBelowMaximum(
    invalidInts:     List[String],
    lessThanMinimum: List[String],
    belowMaximum:    List[String]
) extends RawDataSuccess {

  override def toLogMessage: LogMessage = LogMessage(this.show)
}

object RawData {
  implicit val showError1: Show[NoValidInts] = Show.show(err1 => s"""Error1: ${err1.invalidInts.mkString(", ")}""")

  implicit val showStage2: Show[ValidIntsFound] =
    Show.show(
      stage2Data => s"""Stage 2: ${stage2Data.validInts.mkString(", ")}
     |Error1: ${stage2Data.invalidInts.mkString(", ")}
   """.stripMargin
    )

  implicit val showError2: Show[NoValuesAboveMinimum] =
    Show.show(
      err2 => s"""Error1: ${err2.invalidInts.mkString(", ")}
         |Error2: ${err2.lessThanMinimum.mkString(", ")}
       """.stripMargin
    )

  implicit val showForwardData: Show[IntsAboveMinimum] = Show.show(
    fd => s"""Data to Forward: ${fd.aboveMinimum.mkString(", ")}
       |Error1: ${fd.invalidInts.mkString(", ")}
       |Error2: ${fd.lessThanMinimum.mkString(", ")}
     """.stripMargin
  )

  implicit val showReply: Show[IntsBelowMaximum] = Show.show(
    reply => s"""Reply: ${reply.belowMaximum.mkString(", ")}
                |Error1: ${reply.invalidInts.mkString(", ")}
                |Error2: ${reply.lessThanMinimum.mkString(", ")}
     """.stripMargin
  )
}
