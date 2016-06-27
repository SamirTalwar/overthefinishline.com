package com.overthefinishline.dashboard

import akka.http.scaladsl.server.{Directive0, Directive1}

trait Credentials {
  def store(userCredentials: UserCredentials): Directive0

  def retrieve: Directive1[Option[UserCredentials]]
}

case class UserCredentials(gitHubAccessToken: AccessToken)
case class AccessToken(value: String, scope: String)
