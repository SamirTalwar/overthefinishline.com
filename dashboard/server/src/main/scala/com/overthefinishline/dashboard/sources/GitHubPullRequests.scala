package com.overthefinishline.dashboard.sources

import akka.actor.Actor
import akka.http.scaladsl.HttpExt

class GitHubPullRequests(http: HttpExt) extends Actor {
  def receive = {
    case _ =>
  }
}
