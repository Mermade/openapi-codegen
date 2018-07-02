package IO.OpenAPI.Api

import argonaut._
import argonaut.EncodeJson._
import argonaut.DecodeJson._

import org.http4s.{EntityDecoder, EntityEncoder}
import org.http4s.argonaut._
import org.joda.time.DateTime
import Order._

case class Order (
  id: Option[int],
petId: Option[int],
quantity: Option[int],
shipDate: Option[string],
/* Order Status */
  status: Option[Order.statusEnum],
complete: Option[bool])

object Order {
  import DateTimeCodecs._
  sealed trait Order.statusEnum

  object Order.statusEnum {
    def toOrder.statusEnum(s: String): Option[Order.statusEnum] = s match {
      case _ => None
    }

    def fromOrder.statusEnum(x: Order.statusEnum): String = x match {
    }
  }

  implicit val Order.statusEnumEnumEncoder: EncodeJson[Order.statusEnum] =
    EncodeJson[Order.statusEnum](is => StringEncodeJson(Order.statusEnum.fromOrder.statusEnum(is)))

  implicit val Order.statusEnumEnumDecoder: DecodeJson[Order.statusEnum] =
    DecodeJson.optionDecoder[Order.statusEnum](n => n.string.flatMap(jStr => Order.statusEnum.toOrder.statusEnum(jStr)), "Order.statusEnum failed to de-serialize")

  implicit val OrderCodecJson: CodecJson[Order] = CodecJson.derive[Order]
  implicit val OrderDecoder: EntityDecoder[Order] = jsonOf[Order]
  implicit val OrderEncoder: EntityEncoder[Order] = jsonEncoderOf[Order]
}
import Category._

case class Category (
  id: Option[int],
name: Option[string])

object Category {
  import DateTimeCodecs._

  implicit val CategoryCodecJson: CodecJson[Category] = CodecJson.derive[Category]
  implicit val CategoryDecoder: EntityDecoder[Category] = jsonOf[Category]
  implicit val CategoryEncoder: EntityEncoder[Category] = jsonEncoderOf[Category]
}
import User._

case class User (
  id: Option[int],
username: Option[string],
firstName: Option[string],
lastName: Option[string],
email: Option[string],
password: Option[string],
phone: Option[string],
/* User Status */
  userStatus: Option[int])

object User {
  import DateTimeCodecs._

  implicit val UserCodecJson: CodecJson[User] = CodecJson.derive[User]
  implicit val UserDecoder: EntityDecoder[User] = jsonOf[User]
  implicit val UserEncoder: EntityEncoder[User] = jsonEncoderOf[User]
}
import Tag._

case class Tag (
  id: Option[int],
name: Option[string])

object Tag {
  import DateTimeCodecs._

  implicit val TagCodecJson: CodecJson[Tag] = CodecJson.derive[Tag]
  implicit val TagDecoder: EntityDecoder[Tag] = jsonOf[Tag]
  implicit val TagEncoder: EntityEncoder[Tag] = jsonEncoderOf[Tag]
}
import Pet._

case class Pet (
  id: Option[int],
category: Option[struct{}],
name: string,
photoUrls: [100]string,
tags: Option[[100]struct{}],
/* pet status in the store */
  status: Option[Pet.statusEnum])

object Pet {
  import DateTimeCodecs._
  sealed trait Pet.statusEnum

  object Pet.statusEnum {
    def toPet.statusEnum(s: String): Option[Pet.statusEnum] = s match {
      case _ => None
    }

    def fromPet.statusEnum(x: Pet.statusEnum): String = x match {
    }
  }

  implicit val Pet.statusEnumEnumEncoder: EncodeJson[Pet.statusEnum] =
    EncodeJson[Pet.statusEnum](is => StringEncodeJson(Pet.statusEnum.fromPet.statusEnum(is)))

  implicit val Pet.statusEnumEnumDecoder: DecodeJson[Pet.statusEnum] =
    DecodeJson.optionDecoder[Pet.statusEnum](n => n.string.flatMap(jStr => Pet.statusEnum.toPet.statusEnum(jStr)), "Pet.statusEnum failed to de-serialize")

  implicit val PetCodecJson: CodecJson[Pet] = CodecJson.derive[Pet]
  implicit val PetDecoder: EntityDecoder[Pet] = jsonOf[Pet]
  implicit val PetEncoder: EntityEncoder[Pet] = jsonEncoderOf[Pet]
}
import ApiResponse._

case class ApiResponse (
  code: Option[int],
Type: Option[string],
message: Option[string])

object ApiResponse {
  import DateTimeCodecs._

  implicit val ApiResponseCodecJson: CodecJson[ApiResponse] = CodecJson.derive[ApiResponse]
  implicit val ApiResponseDecoder: EntityDecoder[ApiResponse] = jsonOf[ApiResponse]
  implicit val ApiResponseEncoder: EntityEncoder[ApiResponse] = jsonEncoderOf[ApiResponse]
}
