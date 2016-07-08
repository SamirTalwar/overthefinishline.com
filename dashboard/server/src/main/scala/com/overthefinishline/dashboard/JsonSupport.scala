package com.overthefinishline.dashboard

import java.time.Instant

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import spray.json._

trait JsonSupport extends SprayJsonSupport with DefaultJsonProtocol {
  implicit val accessTokenJsonFormat = jsonFormat2(AccessToken)
  implicit val userCredentialsRootJsonFormat = rootFormat(jsonFormat1(UserCredentials))

  implicit object InstantJsonFormat extends JsonFormat[Instant] {
    override def write(instant: Instant): JsValue = JsNumber(instant.toEpochMilli)

    override def read(value: JsValue): Instant = Instant.ofEpochMilli(value.convertTo[Long])
  }

  implicit object ModelJsonFormat extends RootJsonFormat[Model] {
    implicit val unauthorizedJsonFormat = jsonFormat0(() => Unauthorized)
    implicit val repositoryJsonFormat = jsonFormat3(Repository)
    implicit val pullRequestJsonFormat = jsonFormat5(PullRequest)
    implicit val dashboardJsonFormat = jsonFormat2(Dashboard)

    def write(model: Model) = model match {
      case self @ Unauthorized => JsObject("type" -> "Unauthorized".toJson, "value" -> unauthorizedJsonFormat.write(self))
      case self @ Dashboard(_, _) => JsObject("type" -> "Dashboard".toJson, "value" -> dashboardJsonFormat.write(self))
    }

    def read(value: JsValue) = value match {
      case o: JsObject if o.fields("type") == "Unauthorized".toJson => unauthorizedJsonFormat.read(o.fields("value"))
      case o: JsObject if o.fields("type") == "Dashboard".toJson => dashboardJsonFormat.read(o.fields("value"))
    }
  }
}
