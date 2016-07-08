package com.overthefinishline.dashboard

import akka.http.scaladsl.server.Route
import akka.http.scaladsl.server.Directives._

trait ApplicationRoutes {
  def routes: Route
}

object ApplicationRoutes {
  def apply(theRoutes: ApplicationRoutes*) = new ApplicationRoutes {
    override val routes = theRoutes.map(_.routes).reduce(_ ~ _)
  }
}
