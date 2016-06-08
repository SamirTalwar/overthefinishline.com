package com.overthefinishline.dashboard

import java.nio.file.{Path, Paths}
import scala.collection.JavaConverters._

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import akka.stream.ActorMaterializer
import com.google.api.client.auth.oauth2.{AuthorizationCodeFlow, BearerToken, ClientParametersAuthentication}
import com.google.api.client.http.javanet.NetHttpTransport
import com.google.api.client.http._
import com.google.api.client.json.jackson.JacksonFactory
import spray.json.DefaultJsonProtocol

class Application(clientPath: Path, gitHubOAuthCredentials: ClientParametersAuthentication) extends SprayJsonSupport with DefaultJsonProtocol {
  val gitHubAuthorizationFlow = new AuthorizationCodeFlow.Builder(
    BearerToken.authorizationHeaderAccessMethod(),
    new NetHttpTransport,
    new JacksonFactory,
    new GenericUrl("https://github.com/login/oauth/access_token"),
    gitHubOAuthCredentials,
    gitHubOAuthCredentials.getClientId,
    "https://github.com/login/oauth/authorize")
    .build()

  val requestJson = new HttpRequestInitializer {
    override def initialize(request: HttpRequest): Unit = {
      request.setHeaders(new HttpHeaders().setAccept("application/json"))
    }
  }

  val routes =
    pathSingleSlash {
      encodeResponse {
        getFromFile(clientPath.resolve("index.html").toFile)
      }
    } ~
    encodeResponse {
      getFromDirectory(clientPath.toString)
    } ~
    path("authentication" / "by" / "github") {
      val authorizationUri = gitHubAuthorizationFlow.newAuthorizationUrl().setScopes(Seq("user:email", "repo").asJava)
        .build()
      redirect(authorizationUri, StatusCodes.TemporaryRedirect)
    } ~
    (path("authorization" / "by" / "github") & parameter('code)) { code =>
      val token = gitHubAuthorizationFlow.newTokenRequest(code).setRequestInitializer(requestJson).execute()
      complete(token.toString)
    }
}

object Application {
  def main(args: Array[String]): Unit = {
    implicit val system = ActorSystem("application")
    implicit val materializer = ActorMaterializer()
    implicit val executionContext = system.dispatcher

    val port = System.getenv("PORT").toInt
    val clientPath = Paths.get(System.getenv("CLIENT_PATH"))
    val gitHubOAuthCredentials = new ClientParametersAuthentication(
      System.getenv("GITHUB_OAUTH_CLIENT_ID"),
      System.getenv("GITHUB_OAUTH_CLIENT_SECRET"))
    val bindingFuture = Http().bindAndHandle(new Application(clientPath, gitHubOAuthCredentials).routes, "localhost", port)

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
