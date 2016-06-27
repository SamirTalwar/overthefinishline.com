package com.overthefinishline.dashboard

import java.security.SecureRandom
import javax.crypto.spec.SecretKeySpec

import akka.http.javadsl.model.headers.SetCookie
import akka.http.scaladsl.model.headers.{Cookie, `Set-Cookie`}
import akka.http.scaladsl.model.{HttpResponse, StatusCodes}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.testkit.ScalatestRouteTest
import org.scalatest.{BeforeAndAfter, FunSpec, Matchers}

class EncryptedCookieCredentialsTest extends FunSpec with Matchers with BeforeAndAfter with ScalatestRouteTest with JsonSupport {
  val encryptionKey = new SecretKeySpec("0123456789abcdef".getBytes, "AES")
  val credentials = new EncryptedCookieCredentials(new SecureRandom(), encryptionKey)

  val routes =
    path("store") {
      put {
        entity(as[UserCredentials]) {
          credentials.store(_) {
            complete(HttpResponse(StatusCodes.OK))
          }
        }
      }
    } ~
    path("retrieve") {
      get {
        credentials.retrieve { userCredentials =>
          complete(userCredentials.get)
        }
      }
    }

  describe("storing and retrieving credentials") {
    it("asks the client to log in if no token is found") {
      val entity = new UserCredentials(new AccessToken("value", "scope"))
      Put("/store", entity) ~> routes ~> check {
        response.status should be(StatusCodes.OK)

        val cookie = response.header[`Set-Cookie`].get.cookie
        cookie.name should be("data")
        cookie.value shouldNot be(empty)
        cookie.path should be(Some("/"))

        Get("/retrieve").addHeader(Cookie(cookie.pair)) ~> routes ~> check {
          responseAs[UserCredentials] should be(entity)
        }
      }
    }
  }
}
