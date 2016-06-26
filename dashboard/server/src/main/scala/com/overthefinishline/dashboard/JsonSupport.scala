package com.overthefinishline.dashboard

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import spray.json._

trait JsonSupport extends SprayJsonSupport with DefaultJsonProtocol {
  implicit val accessTokenJsonFormat = jsonFormat2(AccessToken)
  implicit val userCredentialsRootJsonFormat = rootFormat(jsonFormat1(UserCredentials))

  implicit object ModelJsonFormat extends RootJsonFormat[Model] {
    val unauthorizedJsonFormat = jsonFormat0(() => Unauthorized)
    val dashboardJsonFormat = jsonFormat0(() => Dashboard)

    def write(model: Model) = model match {
      case self @ Unauthorized => JsObject("type" -> "Unauthorized".toJson, "value" -> unauthorizedJsonFormat.write(self))
      case self @ Dashboard => JsObject("type" -> "Dashboard".toJson, "value" -> dashboardJsonFormat.write(self))
    }

    def read(value: JsValue) = value match {
      case o: JsObject if o.fields("type") == "Unauthorized".toJson => unauthorizedJsonFormat.read(o.fields("value"))
      case o: JsObject if o.fields("type") == "Dashboard".toJson => dashboardJsonFormat.read(o.fields("value"))
    }
  }
}
