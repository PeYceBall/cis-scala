package seminars.seminar11

import cats.effect.IO
import com.twitter.finagle.Http
import io.finch._

object Server {

  def run(modules: Modules): IO[Nothing] =
    IO(
      Http.server.serve(":8080", (modules.math.api :+: modules.elections.api).rescue{
        case exception: Exception =>
          IO(println(s"Error occured: ${exception.getMessage}")).as(InternalServerError(exception))
      }.toServiceAs[Text.Plain])
    ) *>
      IO.never
}
