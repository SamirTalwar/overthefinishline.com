package com.overthefinishline.dashboard

import java.nio.file.{Path, Paths}
import java.security.SecureRandom
import java.time.Clock
import java.util.Base64
import javax.crypto.spec.SecretKeySpec

import akka.actor.{ActorSystem, Props}
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.stream.ActorMaterializer
import com.google.api.client.auth.oauth2.ClientParametersAuthentication
import com.overthefinishline.dashboard.sources.GitHubPullRequests

object Application {
  val JwtSigningKeyAlgorithm = "HmacSHA512"
  val SecretKeyAlgorithm = "AES"

  def main(args: Array[String]): Unit = {
    implicit val system = ActorSystem("application")
    implicit val materializer = ActorMaterializer()
    implicit val executionContext = system.dispatcher

    val http = Http()
    val base64Decoder = Base64.getDecoder
    val port = System.getenv("PORT").toInt
    val clientPath = Paths.get(System.getenv("CLIENT_PATH"))
    val credentials = new EncryptedCookieCredentials(new SecureRandom, new SecretKeySpec(base64Decoder.decode(System.getenv("ENCRYPTION_KEY")), SecretKeyAlgorithm))

    val staticFileRoute = StaticFileRoute(clientPath)
    val oAuthRoute = OAuthRoute(
      credentials,
      new ClientParametersAuthentication(
        System.getenv("GITHUB_OAUTH_CLIENT_ID"),
        System.getenv("GITHUB_OAUTH_CLIENT_SECRET"))
    )
    val dashboardRoute = DashboardRoute(
      executionContext = executionContext,
      credentials = credentials,
      clock = Clock.systemUTC(),
      gitHubPullRequests = system.actorOf(Props(new GitHubPullRequests(http))))

    val bindingFuture = http.bindAndHandle(
      handler = staticFileRoute ~ oAuthRoute ~ dashboardRoute,
      interface = "localhost",
      port = port)

    println(s"Server online at http://localhost:$port")

    Runtime.getRuntime.addShutdownHook(new Thread(new Runnable {
      override def run(): Unit = {
        bindingFuture
          .flatMap(_.unbind()) // trigger unbinding from the port
          .onComplete(_ => system.terminate()) // and shutdown when done
      }
    }))
  }
}
