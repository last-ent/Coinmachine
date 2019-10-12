package chainer

import cats.effect.Effect
import chainer.models.LogMessage

object Logger {
  def publish[F[_]: Effect](message: LogMessage): F[Unit] = ???
}
