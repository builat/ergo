package org.ergoplatform.api

import cats.syntax.either._
import io.circe._
import io.circe.syntax._
import org.ergoplatform.ErgoBox.TokenId
import org.ergoplatform.nodeView.history.ErgoHistory.Difficulty
import org.ergoplatform.settings.Algos
import scorex.core._
import scorex.core.validation.ValidationResult
import scorex.crypto.authds.ADKey
import scorex.crypto.hash.Digest32
import sigmastate.Values.{EvaluatedValue, Value}
import sigmastate.serialization.ValueSerializer
import sigmastate.{SBoolean, SType}

import scala.util.Try

trait ApiCodecs {

  def fromTry[T](tryResult: Try[T])(implicit cursor: ACursor): Either[DecodingFailure, T] = {
    tryResult.fold(e => Left(DecodingFailure(e.toString, cursor.history)), Right.apply)
  }

  def fromOption[T](maybeResult: Option[T])(implicit cursor: ACursor): Either[DecodingFailure, T] = {
    maybeResult.fold[Either[DecodingFailure, T]](Left(DecodingFailure("No value found", cursor.history)))(Right.apply)
  }

  def fromThrows[T](throwsBlock: => T)(implicit cursor: ACursor): Either[DecodingFailure, T] = {
    Either.catchNonFatal(throwsBlock).leftMap(e => DecodingFailure(e.toString, cursor.history))
  }

  def fromValidation[T](validationResult: ValidationResult[T])
                       (implicit cursor: ACursor): Either[DecodingFailure, T] = {
    fromTry(validationResult.toTry)
  }

  implicit val bigIntEncoder: Encoder[BigInt] = { bigInt =>
    JsonNumber.fromDecimalStringUnsafe(bigInt.toString).asJson
  }

  implicit val difficultyEncoder: Encoder[Difficulty] = bigIntEncoder

  implicit val bytesEncoder: Encoder[Array[Byte]] =  Algos.encode(_).asJson

  implicit val bytesDecoder: Decoder[Array[Byte]] = bytesDecoder(x => x)

  implicit val byteSeqEncoder: Encoder[IndexedSeq[Byte]] = { in =>
    Algos.encode(in.toArray).asJson
  }

  implicit val modifierIdEncoder: Encoder[ModifierId] = _.toString.asJson

  implicit val modifierIdDecoder: Decoder[ModifierId] = ModifierId @@ _.as[String]

  implicit val tokenIdEncoder: Encoder[TokenId] = _.array.asJson

  implicit val tokenIdDecoder: Decoder[TokenId] = bytesDecoder(Digest32 @@ _)

  implicit val adKeyEncoder: Encoder[ADKey] = _.array.asJson

  implicit val adKeyDecoder: Decoder[ADKey] = bytesDecoder(ADKey @@  _)

  def bytesDecoder[T](transform: Array[Byte] => T): Decoder[T] = { implicit cursor =>
    for {
      str <- cursor.as[String]
      bytes <- fromTry(Algos.decode(str))
    } yield transform(bytes)
  }

  implicit val valueEncoder: Encoder[Value[SType]] = { value =>
    ValueSerializer.serialize(value).asJson
  }

  implicit val booleanValueEncoder: Encoder[Value[SBoolean.type]] = { value =>
    valueEncoder(value)
  }

  implicit val booleanValueDecoder: Decoder[Value[SBoolean.type]] = {
    valueDecoder(_.asInstanceOf[Value[SBoolean.type]])
  }

  implicit val evaluatedValueDecoder: Decoder[EvaluatedValue[SType]] = {
    valueDecoder(_.asInstanceOf[EvaluatedValue[SType]])
  }

  def valueDecoder[T](transform: Value[SType] => T): Decoder[T]  = { implicit cursor: ACursor =>
    cursor.as[Array[Byte]] flatMap { bytes =>
      fromThrows(transform(ValueSerializer.deserialize(bytes)))
    }
  }

}
