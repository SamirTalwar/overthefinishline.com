package com.overthefinishline.dashboard

import akka.http.scaladsl.server.Route
import akka.http.scaladsl.server.directives.RouteDirectives

class NullRoutes extends ApplicationRoutes {
  override val routes: Route = RouteDirectives.reject
}
