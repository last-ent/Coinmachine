package donatron

import cats.effect.IO
import donatron.models._
import org.scalatest.{EitherValues, Matchers, WordSpec}

class DonatronSpec extends WordSpec with Matchers with EitherValues {

  val chainMachine: Donatron[IO] = new Donatron[IO]

  def getResponse(req: Request): Response = chainMachine.donate(req).unsafeRunSync()

  "" should {

    "return Response message when Request has valid ints between 10 & 10k" in {
      val req = Request((10 to 20).map(_.toString).toList)

      val expectedResponse =
        IntsBelowMaximum(
          invalidInts = List.empty,
          lessThanMinimum = List.empty,
          belowMaximum   = req.values.map(v => s"Valid Number: $v")
        ).toResponse

      getResponse(req) shouldEqual expectedResponse
    }

    "return Response message when Request has few invalid ints" in {
      val req = Request(List("10", "20", "10a", "asdf", "zzz124"))

      val expectedResponse =
        IntsBelowMaximum(
          invalidInts = req.values.drop(2),
          lessThanMinimum = List.empty,
          belowMaximum   = req.values.take(2).map(v => s"Valid Number: $v")
        ).toResponse

      getResponse(req) shouldEqual expectedResponse
    }

    "return Response message when Request has few valid ints less than 10" in {
      val req = Request((7 to 20).map(_.toString).toList)

      val expectedResponse =
        IntsBelowMaximum(
          invalidInts = List.empty,
          lessThanMinimum = req.values.take(3),
          belowMaximum = req.values.drop(3).map(v => s"Valid Number: $v")
        ).toResponse

      getResponse(req) shouldEqual expectedResponse
    }

    "return NoValidInts message when Request has no valid ints" in {
      val req = Request(List("10a", "asdf", "zzz124"))

      getResponse(req) shouldEqual NoValidInts(invalidInts = req.values).toResponse
    }

    "return NoValuesAboveMinimum message when Request only has valid ints less than 10" in {
      val req = Request(List("1", "2"))

      val expectedResponse =
        NoValuesAboveMinimum(
          invalidInts = List.empty,
          lessThanMinimum = req.values
        ).toResponse

      getResponse(req) shouldEqual expectedResponse
    }

    "return Exception when Request has valid ints greater than 10k" in {
      val req = Request((9999 to 10000).map(_.toString).toList)

      chainMachine.donate(req).attempt.unsafeRunSync().left.value.getMessage shouldEqual "Uh oh!"
    }
  }
}
