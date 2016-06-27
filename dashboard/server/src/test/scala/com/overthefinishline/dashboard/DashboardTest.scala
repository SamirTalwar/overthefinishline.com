package com.overthefinishline.dashboard

import java.nio.file.Paths

import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import org.scalatest.{BeforeAndAfter, FunSpec, Matchers}

class DashboardTest extends FunSpec with Matchers with BeforeAndAfter with ScalatestRouteTest with JsonSupport {
  var routes: Route = null

  before {
    routes = new Application(
      clientPath = Paths.get(""),
      oAuthRoutes = new NullRoutes,
      credentials = new FakeCredentials
    ).routes
  }

  describe("the dashboard") {
    it("asks the client to log in if no token is found") {
      Get("/dashboard") ~> routes ~> check {
        responseAs[Model] should be(Unauthorized)
      }
    }
  }
}
