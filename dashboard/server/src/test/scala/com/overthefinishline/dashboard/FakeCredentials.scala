package com.overthefinishline.dashboard

import akka.http.scaladsl.server.directives.BasicDirectives._
import akka.http.scaladsl.server.{Directive0, Directive1}

class FakeCredentials extends Credentials {
  var credentials: Option[UserCredentials] = None

  override def store(userCredentials: UserCredentials): Directive0 = pass

  override def retrieve: Directive1[Option[UserCredentials]] = provide(credentials)
}
