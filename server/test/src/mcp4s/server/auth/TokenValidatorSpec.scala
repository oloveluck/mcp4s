package mcp4s.server.auth

import cats.effect.IO
import mcp4s.protocol.{AuthError, TokenInfo}
import munit.CatsEffectSuite

class TokenValidatorSpec extends CatsEffectSuite:

  val validator: TokenValidator[IO] = TokenValidator.jwt[IO]

  // Helper to create a JWT with the given payload (no signature verification in dev mode)
  private def makeJwt(payload: String): String =
    val header = java.util.Base64.getUrlEncoder.withoutPadding().encodeToString(
      """{"alg":"HS256","typ":"JWT"}""".getBytes("UTF-8")
    )
    val encodedPayload = java.util.Base64.getUrlEncoder.withoutPadding().encodeToString(
      payload.getBytes("UTF-8")
    )
    s"$header.$encodedPayload.signature"

  test("jwt validator parses valid JWT with all standard claims") {
    val payload = """{
      "sub": "user123",
      "aud": "my-api",
      "iss": "https://auth.example.com",
      "exp": 1735689600,
      "scope": "read write admin"
    }"""
    val token = makeJwt(payload)

    validator.validate(token).map { result =>
      assert(result.isRight)
      val info = result.toOption.get
      assertEquals(info.subject, "user123")
      assertEquals(info.audience, Some("my-api"))
      assertEquals(info.issuer, Some("https://auth.example.com"))
      assertEquals(info.expiration, Some(1735689600L))
      assertEquals(info.scopes, Set("read", "write", "admin"))
    }
  }

  test("jwt validator extracts space-separated scopes") {
    val payload = """{"sub": "user1", "scope": "mcp:read mcp:write"}"""
    val token = makeJwt(payload)

    validator.validate(token).map { result =>
      assert(result.isRight)
      assertEquals(result.toOption.get.scopes, Set("mcp:read", "mcp:write"))
    }
  }

  test("jwt validator extracts array-format scopes") {
    val payload = """{"sub": "user1", "scopes": ["read", "write", "delete"]}"""
    val token = makeJwt(payload)

    validator.validate(token).map { result =>
      assert(result.isRight)
      assertEquals(result.toOption.get.scopes, Set("read", "write", "delete"))
    }
  }

  test("jwt validator returns error for invalid JWT format") {
    validator.validate("not-a-jwt").map { result =>
      assert(result.isLeft)
      result match
        case Left(AuthError.InvalidToken(msg)) =>
          assert(msg.contains("Invalid JWT format"))
        case _ => fail("Expected InvalidToken error")
    }
  }

  test("jwt validator returns error for invalid base64 encoding") {
    // Create a token with invalid base64 in payload position
    val token = "eyJhbGciOiJIUzI1NiJ9.!!!invalid-base64!!!.signature"

    validator.validate(token).map { result =>
      assert(result.isLeft)
      result match
        case Left(AuthError.InvalidToken(msg)) =>
          assert(msg.contains("Invalid") || msg.contains("base64") || msg.contains("JSON"))
        case _ => fail("Expected InvalidToken error")
    }
  }

  test("jwt validator defaults to 'unknown' subject when sub claim is missing") {
    val payload = """{"aud": "my-api", "scope": "read"}"""
    val token = makeJwt(payload)

    validator.validate(token).map { result =>
      assert(result.isRight)
      assertEquals(result.toOption.get.subject, "unknown")
    }
  }

  test("jwt validator handles empty scopes") {
    val payload = """{"sub": "user1"}"""
    val token = makeJwt(payload)

    validator.validate(token).map { result =>
      assert(result.isRight)
      assertEquals(result.toOption.get.scopes, Set.empty[String])
    }
  }

  test("jwt validator preserves all claims in claims map") {
    val payload = """{"sub": "user1", "custom_claim": "custom_value", "role": "admin"}"""
    val token = makeJwt(payload)

    validator.validate(token).map { result =>
      assert(result.isRight)
      val claims = result.toOption.get.claims
      assert(claims.contains("custom_claim"))
      assert(claims.contains("role"))
    }
  }
