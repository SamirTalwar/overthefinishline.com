package com.overthefinishline.dashboard

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import spray.json._

trait JsonSupport extends SprayJsonSupport with DefaultJsonProtocol {
  implicit object ModelJsonFormat extends RootJsonFormat[Model] {
    val unauthorizedJsonFormat = jsonFormat0(() => Unauthorized)

    def write(model: Model) = model match {
      case self @ Unauthorized => JsObject("type" -> "Unauthorized".toJson, "value" -> unauthorizedJsonFormat.write(self))
    }

    def read(value: JsValue) = value match {
      case o: JsObject if o.fields("type") == "Unauthorized".toJson => unauthorizedJsonFormat.read(o.fields("value"))
    }
  }
}
