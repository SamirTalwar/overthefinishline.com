package com.overthefinishline.dashboard

import java.time.{Clock, Instant, ZoneOffset, ZonedDateTime}

import akka.actor.{Actor, Props}
import akka.dispatch.ExecutionContexts
import akka.http.scaladsl.testkit.ScalatestRouteTest
import org.scalatest.{BeforeAndAfter, FunSpec, Matchers}

class DashboardTest extends FunSpec with Matchers with BeforeAndAfter with ScalatestRouteTest with JsonSupport {
  val now: Instant = ZonedDateTime.of(2001, 2, 3, 4, 5, 6, 0, ZoneOffset.UTC).toInstant

  val credentials = new FakeCredentials
  val clock = Clock.fixed(now, ZoneOffset.UTC)
  val gitHubPullRequests = system.actorOf(Props(new Actor {
    def receive = {
      case _ => sender ! pullRequests
    }
  }))
  val routes = DashboardRoute(
    executionContext = ExecutionContexts.global(),
    credentials = credentials,
    clock = clock,
    gitHubPullRequests = gitHubPullRequests
  )

  var pullRequests: PullRequests = null

  describe("the dashboard") {
    it("asks the client to log in if no token is found") {
      Get("/dashboard") ~> routes ~> check {
        responseAs[Model] should be(Unauthorized)
      }
    }

    it("gets a list of pull requests for a random repository") {
      pullRequests = PullRequests(
        PullRequest(
          repository = Repository(
            owner = "sandwiches",
            name = "cheese",
            link = "https://github.com/sandwiches/cheese"
          ),
          number = 123,
          title = "Add support for French cheese.",
          updatedAt = ZonedDateTime.parse("2016-05-04T15:44:33Z").toInstant,
          link = "https://github.com/sandwiches/cheese/pull/123"
        ),
        PullRequest(
          repository = Repository(
            owner = "sandwiches",
            name = "cheese",
            link = "https://github.com/sandwiches/cheese"
          ),
          number = 121,
          title = "Discontinue pre-sliced cheese wrapped in plastic.",
          updatedAt = ZonedDateTime.parse("2016-02-06T03:08:56Z").toInstant,
          link = "https://github.com/sandwiches/cheese/pull/121"
        )
      )

      credentials.credentials = Some(UserCredentials(AccessToken("", "")))
      Get("/dashboard") ~> routes ~> check {
        responseAs[Model] should be(Dashboard(now, pullRequests))
      }
    }
  }
}
