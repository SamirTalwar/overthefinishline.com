package com.overthefinishline.dashboard

import java.nio.file.Paths
import java.security.SecureRandom
import java.time.Clock
import java.util.Base64
import javax.crypto.spec.SecretKeySpec
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}

import akka.actor.{ActorSystem, Props}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.{Http, HttpExt}
import akka.stream.ActorMaterializer
import com.google.api.client.auth.oauth2.ClientParametersAuthentication
import com.overthefinishline.dashboard.sources.GitHubPullRequests

object Application {
  val JwtSigningKeyAlgorithm = "HmacSHA512"
  val SecretKeyAlgorithm = "AES"

  private lazy val base64Decoder = Base64.getDecoder

  def main(args: Array[String]): Unit = {
    implicit val system = ActorSystem("application")
    implicit val materializer = ActorMaterializer()
    implicit val executionContext = system.dispatcher

    val http = Http()
    val port = System.getenv("PORT").toInt
    val bindingFuture = http.bindAndHandle(routes(system, executionContext, http), "localhost", port)

    println(s"Server online at http://localhost:$port")

    Runtime.getRuntime.addShutdownHook(new Thread(new Runnable {
      override def run(): Unit = {
        bindingFuture
          .flatMap(_.unbind()) // trigger unbinding from the port
          .onComplete(_ => system.terminate()) // and shutdown when done
      }
    }))
  }

  private def routes(system: ActorSystem, executionContext: ExecutionContext, http: HttpExt): Route = {
    val clientPath = Paths.get(System.getenv("CLIENT_PATH"))
    val encryptionKey = base64Decoder.decode(System.getenv("ENCRYPTION_KEY"))
    val credentials = new EncryptedCookieCredentials(
      new SecureRandom,
      new SecretKeySpec(encryptionKey, SecretKeyAlgorithm))

    (
      StaticFileRoute(clientPath)
    ~
      OAuthRoute(
        credentials,
        new ClientParametersAuthentication(
          System.getenv("GITHUB_OAUTH_CLIENT_ID"),
          System.getenv("GITHUB_OAUTH_CLIENT_SECRET")))
    ~
      DashboardRoute(
        executionContext,
        credentials,
        Clock.systemUTC(),
        system.actorOf(Props(new GitHubPullRequests(http))))
    )
  }
}
