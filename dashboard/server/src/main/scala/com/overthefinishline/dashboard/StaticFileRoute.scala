package com.overthefinishline.dashboard

import java.nio.file.Path

import akka.http.scaladsl.server.Directives._

object StaticFileRoute {
  def apply(clientPath: Path) =
    pathSingleSlash {
      encodeResponse {
        getFromFile(clientPath.resolve("index.html").toFile)
      }
    } ~
    encodeResponse {
      getFromDirectory(clientPath.toString)
    }
}
