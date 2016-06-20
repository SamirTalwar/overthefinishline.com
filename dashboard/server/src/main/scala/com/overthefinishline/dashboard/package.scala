package com.overthefinishline

package object dashboard {
  sealed trait Model
  case object Unauthorized extends Model
}
