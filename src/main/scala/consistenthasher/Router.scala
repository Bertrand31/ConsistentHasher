package consistenthasher

import cats.implicits._
import cats.effect.{IO, Sync}
import org.http4s.HttpRoutes
import org.http4s.dsl.io._
import io.circe.generic.auto._
import io.circe.syntax._
import org.http4s.circe._

object Router {

  val controller = new Controller

  implicit val intDecoder = jsonOf[IO, Int]
  implicit val keyValueDecoder = jsonOf[IO, Map[String, String]]

  def routes[F[_]: Sync]: HttpRoutes[IO] = {

    HttpRoutes.of[IO] {

      // case req @ POST -> Root / "init" =>
        // req.as[Int].flatMap(controller.initialize) *> Ok("initialized")

      case req @ POST -> Root / "init" =>
        req.as[Int].flatMap(
          controller.initialize(_) *> Ok("initialized")
        )

      case req @ POST -> Root / "add" =>
        req.as[Map[String, String]].flatMap(
          _
            .toList
            .map({ case (key, value) => controller.add(key, value) })
            .sequence *>
          Ok("added")
        )

      case GET -> Root / "show" =>
        Ok(controller.show.asJson)
    }
  }
}
