package com.overthefinishline

import java.time.Instant

package object dashboard {
  sealed trait Model

  case object Unauthorized extends Model

  case class Dashboard(now: Instant, pullRequests: PullRequests) extends Model

  type PullRequests = Vector[PullRequest]
  val PullRequests = Vector

  case class PullRequest(repository: Repository, number: Int, title: String, updatedAt: Instant, link: Link)

  case class Repository(owner: String, name: String, link: Link)

  type Link = String
}
