package com.overthefinishline.dashboard

import scala.collection.JavaConverters._

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import com.google.api.client.auth.oauth2.{AuthorizationCodeFlow, BearerToken, ClientParametersAuthentication}
import com.google.api.client.http.javanet.NetHttpTransport
import com.google.api.client.http.{GenericUrl, HttpHeaders, HttpRequest, HttpRequestInitializer}
import com.google.api.client.json.jackson.JacksonFactory

object OAuthRoute extends JsonSupport {
  def apply(credentials: Credentials, gitHubOAuthCredentials: ClientParametersAuthentication) = {
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

    path("authentication" / "by" / "github") {
      val authorizationUri = gitHubAuthorizationFlow.newAuthorizationUrl().setScopes(Seq("user:email", "repo").asJava)
        .build()
      redirect(authorizationUri, StatusCodes.TemporaryRedirect)
    } ~
    (path("authorization" / "by" / "github") & parameter('code)) { code =>
      val token = gitHubAuthorizationFlow.newTokenRequest(code).setRequestInitializer(requestJson).execute()
      val userCredentials = UserCredentials(
        gitHubAccessToken = AccessToken(value = token.getAccessToken, scope = token.getScope)
      )
      credentials.store(userCredentials) {
        redirect("/", StatusCodes.TemporaryRedirect)
      }
    }
  }
}
