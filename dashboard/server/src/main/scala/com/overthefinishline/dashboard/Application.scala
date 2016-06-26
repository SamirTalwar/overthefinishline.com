package com.overthefinishline.dashboard

import java.nio.file.{Path, Paths}
import java.security.SecureRandom
import java.util.Base64
import javax.crypto.spec.SecretKeySpec

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives._
import akka.stream.ActorMaterializer
import com.google.api.client.auth.oauth2.ClientParametersAuthentication

class Application(clientPath: Path, credentials: Credentials, oAuthRoutes: ApplicationRoutes) extends JsonSupport {
  lazy val routes = staticFileRoutes ~ dashboardRoutes ~ oAuthRoutes.routes

  private val staticFileRoutes =
    pathSingleSlash {
      encodeResponse {
        getFromFile(clientPath.resolve("index.html").toFile)
      }
    } ~
    encodeResponse {
      getFromDirectory(clientPath.toString)
    }

  private val dashboardRoutes =
    path("dashboard") {
      credentials.retrieve {
        case Some(userCredentials) => complete(Dashboard.asInstanceOf[Model])
        case None => complete(Unauthorized.asInstanceOf[Model])
      }
    }
}

object Application {
  val JwtSigningKeyAlgorithm = "HmacSHA512"
  val SecretKeyAlgorithm = "AES"

  def main(args: Array[String]): Unit = {
    implicit val system = ActorSystem("application")
    implicit val materializer = ActorMaterializer()
    implicit val executionContext = system.dispatcher

    val base64Decoder = Base64.getDecoder
    val port = System.getenv("PORT").toInt
    val clientPath = Paths.get(System.getenv("CLIENT_PATH"))
    val credentials = new Credentials(new SecureRandom, new SecretKeySpec(base64Decoder.decode(System.getenv("ENCRYPTION_KEY")), SecretKeyAlgorithm))
    val oAuthRoutes = new OAuthRoutes(
      credentials,
      new ClientParametersAuthentication(
        System.getenv("GITHUB_OAUTH_CLIENT_ID"),
        System.getenv("GITHUB_OAUTH_CLIENT_SECRET"))
    )
    val application = new Application(clientPath, credentials, oAuthRoutes)
    val bindingFuture = Http().bindAndHandle(application.routes, "localhost", port)

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
