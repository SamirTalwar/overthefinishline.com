package com.overthefinishline.dashboard

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import java.security.SecureRandom
import java.util.Base64
import javax.crypto.spec.GCMParameterSpec
import javax.crypto.{Cipher, SecretKey}

import akka.http.scaladsl.model.headers.HttpCookie
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Directive0, Directive1}
import spray.json._

class Credentials(random: SecureRandom, encryptionKey: SecretKey) extends JsonSupport {
  import Credentials._

  private val base64Decoder = Base64.getDecoder
  private val base64Encoder = Base64.getEncoder

  def store(userCredentials: UserCredentials): Directive0 =
    setCookie(HttpCookie(
      name = CookieName,
      value = encrypt(userCredentials.toJson.compactPrint),
      path = Some("/")))

  def retrieve: Directive1[Option[UserCredentials]] =
    optionalCookie(CookieName).map(_.map(_.value).map(decrypt).map(_.parseJson.convertTo[UserCredentials]))

  private def encrypt(plainText: String): String = {
    val iv = new Array[Byte](InitializationVectorSize)
    random.nextBytes(iv)
    val cipher = createCipher(Cipher.ENCRYPT_MODE, iv)
    val actualCipherText = cipher.doFinal(plainText.getBytes)
    val cipherText = ByteBuffer.allocate(iv.length + actualCipherText.length)
      .put(iv)
      .put(actualCipherText)
    cipherText.rewind()
    StandardCharsets.US_ASCII.decode(base64Encoder.encode(cipherText)).toString
  }

  private def decrypt(cipherText: String): String = {
    val buffer = ByteBuffer.wrap(base64Decoder.decode(cipherText))
    val iv = new Array[Byte](InitializationVectorSize)
    buffer.get(iv, 0, InitializationVectorSize)
    val actualCipherText = new Array[Byte](buffer.remaining())
    buffer.get(actualCipherText)
    val cipher = createCipher(Cipher.DECRYPT_MODE, iv)
    new String(cipher.doFinal(actualCipherText))
  }

  private def createCipher(mode: Int, iv: Array[Byte]): Cipher = {
    val cipher = Cipher.getInstance(CipherTransformation)
    cipher.init(mode, encryptionKey, new GCMParameterSpec(128, iv))
    cipher
  }
}

object Credentials {
  private val CookieName = "data"
  private val CipherTransformation = "AES/GCM/NoPadding"
  private val InitializationVectorSize = 16
}

case class UserCredentials(gitHubAccessToken: AccessToken)
case class AccessToken(value: String, scope: String)
