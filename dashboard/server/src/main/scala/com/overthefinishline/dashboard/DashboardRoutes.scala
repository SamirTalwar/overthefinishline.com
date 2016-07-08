package com.overthefinishline.dashboard

import java.time.Clock
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

import akka.actor.ActorRef
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import akka.util.Timeout

class DashboardRoutes(
  executionContext: ExecutionContext,
  credentials: Credentials,
  clock: Clock,
  gitHubPullRequests: ActorRef
) extends ApplicationRoutes with JsonSupport {
  private implicit val _executionContext = executionContext
  private implicit val timeout = Timeout(5.seconds)

  override val routes: Route =
    path("dashboard") {
      credentials.retrieve {
        case Some(userCredentials) =>
          onSuccess(gitHubPullRequests ? userCredentials)
            .map(_.asInstanceOf[PullRequests])
            .apply(pullRequests => complete(Dashboard(clock.instant(), pullRequests).asInstanceOf[Model]))
        case None => complete(Unauthorized.asInstanceOf[Model])
      }
    }
}
