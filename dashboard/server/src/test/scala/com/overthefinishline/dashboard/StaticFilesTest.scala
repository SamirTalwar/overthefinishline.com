package com.overthefinishline.dashboard

import java.nio.file.{Files, Path}
import java.util.function.Consumer

import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import org.scalatest.{BeforeAndAfter, FunSpec, Matchers}

class StaticFilesTest extends FunSpec with Matchers with BeforeAndAfter with ScalatestRouteTest {
  val port = 8099
  var clientPath: Path = null
  var routes: Route = null

  before {
    clientPath = Files.createTempDirectory("public")
    routes = new Application(
      clientPath = clientPath,
      applicationRoutes = new NullRoutes
    ).routes
  }

  after {
    delete(clientPath)
  }

  describe("the static files served") {
    it("serves a file named index.html from the root") {
      val contents = "<h1>Hello, world!</h1>"
      Files.write(clientPath.resolve("index.html"), contents.getBytes)
      Get("/") ~> routes ~> check {
        responseAs[String] should be(contents)
      }
    }

    it("serves other static files") {
      val contents = "function() { console.log('This is some JavaScript.'); }"
      Files.write(clientPath.resolve("stuff.js"), contents.getBytes)
      Get("/stuff.js") ~> routes ~> check {
        responseAs[String] should be(contents)
      }
    }
  }

  def delete(path: Path) {
    if (Files.isDirectory(path))
      Files.list(path).forEach(new Consumer[Path] {
        override def accept(child: Path): Unit = delete(child)
      })
    Files.delete(path)
  }
}
