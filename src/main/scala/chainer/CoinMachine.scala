package chainer

import cats.data.EitherT
import cats.effect.Effect
import cats.implicits._
import chainer.models._

import scala.util.Try

class CoinMachine[F[_]: Effect]() {

  def insert(request: Request): F[Response] =
    checkForValidInts(request)
      .flatMap(checkForMinimumLength)
      .flatMap(forwardRequest)
      .flatMap(logReply)
      .merge
      .flatMap(logAndReturnResponse)

  def checkForValidInts(req: Request): EitherT[F, RawData, ValidIntsFound] = {
    val parts = req.values.partition(value => Try(value.toInt).isSuccess)

    val asEither = parts match {
      case (noValidInts: List[String], allNonInts: List[String]) if noValidInts.isEmpty =>
        Left(NoValidInts(invalidInts = allNonInts))

      case (validInts: List[String], nonInts: List[String]) =>
        Right(ValidIntsFound(invalidInts = nonInts, validInts = validInts))
    }

    EitherT.fromEither(asEither)
  }

  def checkForMinimumLength(data: ValidIntsFound): EitherT[F, RawData, IntsAboveMinimum] = {
    val asEither = data.validInts.partition(_.length > 1) match {
      case (noneAboveMinimum: List[String], allBelowMinimum: List[String]) if noneAboveMinimum.isEmpty =>
        Left(NoValuesAboveMinimum(invalidInts = data.invalidInts, lessThanMinimum = allBelowMinimum))

      case (aboveMinimum: List[String], belowMinimum: List[String]) =>
        Right(IntsAboveMinimum(invalidInts = data.invalidInts, lessThanMinimum = belowMinimum, aboveMinimum = aboveMinimum))
    }

    EitherT.fromEither(asEither)
  }

  def forwardRequest(data: IntsAboveMinimum): EitherT[F, RawData, IntsBelowMaximum] =
    EitherT.liftF(
      checkForMaximumLength(data.aboveMinimum).map { replyData =>
        IntsBelowMaximum(
          invalidInts     = data.invalidInts,
          lessThanMinimum = data.lessThanMinimum,
          belowMaximum    = replyData
        )
      }
    )

  def checkForMaximumLength(data: List[String]): F[List[String]] =
    data.traverse { value =>
      value.length match {
        case x if x >= 5 => Effect[F].raiseError(new RuntimeException("Uh oh!"))
        case _           => Effect[F].pure(s"Valid Number: $value")
      }
    }

  def logAndReturnResponse(data: RawData): F[Response] = logResponse(data) >> Effect[F].delay(data.toResponse)

  def logReply(reply: IntsBelowMaximum): EitherT[F, RawData, IntsBelowMaximum] = EitherT.rightT(reply)

  def logResponse(data: RawData): F[Unit] = Effect[F].unit
}
