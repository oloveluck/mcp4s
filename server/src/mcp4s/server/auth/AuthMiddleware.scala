package mcp4s.server.auth

import cats.data.{Kleisli, OptionT}
import cats.effect.Concurrent
import cats.syntax.all.*
import io.circe.syntax.*
import org.http4s.*
import org.http4s.circe.*
import org.http4s.dsl.Http4sDsl
import org.http4s.headers.{Authorization, `Content-Type`, `WWW-Authenticate`}
import org.typelevel.vault.Key
import mcp4s.protocol.{AuthError, Codecs, TokenInfo}
import Codecs.given

/** HTTP middleware for OAuth bearer token authentication.
  *
  * Implements RFC 6750 and RFC 9728 for protected resource authentication.
  */
object AuthMiddleware:

  /** Request vault key for storing validated TokenInfo. */
  def tokenInfoKey[F[_]: Concurrent]: F[Key[TokenInfo]] = Key.newKey[F, TokenInfo]

  /** Extension methods for accessing token info from requests. */
  extension [F[_]](req: Request[F])
    /** Get the validated token info from the request, if authenticated. */
    def tokenInfo(using key: Key[TokenInfo]): Option[TokenInfo] =
      req.attributes.lookup(key)

  /** Create auth middleware that protects routes.
    *
    * @param config Auth configuration with metadata and validator
    * @param routes The routes to protect
    * @param key Vault key for storing TokenInfo
    * @return Protected routes that validate bearer tokens and return 401/403 on failure
    */
  def apply[F[_]: Concurrent](
      config: AuthConfig[F],
      routes: HttpRoutes[F]
  )(using key: Key[TokenInfo]): HttpRoutes[F] =
    Kleisli { req =>
      extractBearerToken(req) match
        case None =>
          // No token - return 401 with WWW-Authenticate header
          OptionT.liftF(unauthorizedResponse[F](config))

        case Some(token) =>
          OptionT.liftF(config.validator.validate(token)).flatMap {
            case Left(error) =>
              // Invalid token - return appropriate error response
              OptionT.liftF(errorResponse[F](config, error))

            case Right(tokenInfo) =>
              // Check required scopes
              if config.requiredScopes.subsetOf(tokenInfo.scopes) || config.requiredScopes.isEmpty then
                // Token valid and has required scopes - attach to request and continue
                val enrichedReq = req.withAttribute(key, tokenInfo)
                routes(enrichedReq)
              else
                // Insufficient scopes - return 403
                OptionT.liftF(forbiddenResponse[F](config))
          }
    }

  /** Create routes for the well-known metadata endpoint.
    *
    * Exposes `/.well-known/oauth-protected-resource` per RFC 9728.
    * This endpoint does NOT require authentication.
    *
    * @param config Auth configuration with metadata
    * @return Routes for the metadata endpoint
    */
  def metadataRoutes[F[_]: Concurrent](config: AuthConfig[F]): HttpRoutes[F] =
    val dsl = new Http4sDsl[F] {}
    import dsl.*

    HttpRoutes.of[F] {
      case GET -> Root / ".well-known" / "oauth-protected-resource" =>
        Ok(config.metadata.asJson, `Content-Type`(MediaType.application.json))
    }

  /** Extract bearer token from Authorization header. */
  private def extractBearerToken[F[_]](req: Request[F]): Option[String] =
    req.headers.get[Authorization].flatMap { auth =>
      auth.credentials match
        case Credentials.Token(AuthScheme.Bearer, token) => Some(token)
        case _ => None
    }

  /** Build WWW-Authenticate challenge header. */
  private def wwwAuthenticateChallenge(
      resource: String,
      params: Map[String, String]
  ): `WWW-Authenticate` =
    `WWW-Authenticate`(Challenge("Bearer", resource, params))

  /** Create 401 Unauthorized response with WWW-Authenticate header. */
  private def unauthorizedResponse[F[_]: Concurrent](config: AuthConfig[F]): F[Response[F]] =
    Response[F](Status.Unauthorized)
      .putHeaders(wwwAuthenticateChallenge(config.metadata.resource, Map("resource_metadata" -> config.metadataUri)))
      .pure[F]

  /** Create 401 Unauthorized response with error details. */
  private def unauthorizedWithError[F[_]: Concurrent](
      config: AuthConfig[F],
      error: String,
      description: String
  ): F[Response[F]] =
    Response[F](Status.Unauthorized)
      .putHeaders(
        wwwAuthenticateChallenge(
          config.metadata.resource,
          Map("error" -> error, "error_description" -> description)
        )
      )
      .pure[F]

  /** Create 403 Forbidden response with insufficient scope error. */
  private def forbiddenResponse[F[_]: Concurrent](config: AuthConfig[F]): F[Response[F]] =
    Response[F](Status.Forbidden)
      .putHeaders(
        wwwAuthenticateChallenge(
          config.metadata.resource,
          Map("error" -> "insufficient_scope", "scope" -> config.requiredScopes.mkString(" "))
        )
      )
      .pure[F]

  /** Create appropriate error response for auth errors. */
  private def errorResponse[F[_]: Concurrent](
      config: AuthConfig[F],
      error: AuthError
  ): F[Response[F]] =
    error match
      case AuthError.MissingCredentials(_) =>
        unauthorizedResponse[F](config)

      case AuthError.InvalidToken(reason) =>
        unauthorizedWithError[F](config, "invalid_token", reason)

      case AuthError.TokenExpired(_) =>
        unauthorizedWithError[F](config, "invalid_token", "Token expired")

      case AuthError.InvalidAudience(_, _) =>
        unauthorizedWithError[F](config, "invalid_token", "Invalid audience")

      case AuthError.InsufficientScope(required, _) =>
        Response[F](Status.Forbidden)
          .putHeaders(
            wwwAuthenticateChallenge(
              config.metadata.resource,
              Map("error" -> "insufficient_scope", "scope" -> required.mkString(" "))
            )
          )
          .pure[F]
