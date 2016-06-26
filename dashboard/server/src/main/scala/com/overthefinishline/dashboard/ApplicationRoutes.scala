package com.overthefinishline.dashboard

import akka.http.scaladsl.server.Route

trait ApplicationRoutes {
  val routes: Route
}
