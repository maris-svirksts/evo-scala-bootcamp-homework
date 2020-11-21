package http

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits.catsSyntaxFlatMapOps
import org.http4s._
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.server.blaze.BlazeServerBuilder

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext
import scala.util.Random

// Homework. Place the solution under `http` package in your homework repository.
//
// Write a server and a client that play a number guessing game together.
//
// Communication flow should be as follows:
// 1. The client asks the server to start a new game by providing the minimum and the maximum number that can
//    be guessed, as well as the maximum number of attempts.
// 2. The server comes up with some random number within the provided range.
// 3. The client starts guessing the number. Upon each attempt, the server evaluates the guess and responds to
//    the client, whether the current number is lower, greater or equal to the guessed one.
// 4. The game ends when the number is guessed or there are no more attempts left. At this point the client
//    should terminate, while the server may continue running forever.
// 5. The server should support playing many separate games (with different clients) at the same time.
//
// Use HTTP or WebSocket for communication. The exact protocol and message format to use is not specified and
// should be designed while working on the task.

object GuessServer extends IOApp {

  private val gameRoutes = HttpRoutes.of[IO] {
    /*
     * Using 'pretty' permalinks instead of GET or POST parameters because that looks better.
     *
     * Assume that the user is logged in and validated. That allows us to play multiple games within one browser at the same time.
     * Would not need the part above if assumed that each client uses their own browser because all data is saved on client side: in cookies.
     *
     * In production code would save that data on server side, in database.
     *
     * We set new values within cookies. If the specific user already had a game going, the data for it is reset.
     */

    /*
     Welcome user, setup or reset game.

     http://127.0.0.1:9001/user/maris/min/3/max/9/guesses/3
     */
    case GET -> Root / "user" / name / "min" / min / "max" / max / "guesses" / guesses =>
      val welcome = s"Hello, ${name.capitalize}! You want to play a guessing game with numbers from $min to $max and the amount of guesses set to $guesses."

      val r           = scala.util.Random
      val serverGuess = r.between(min.toInt, max.toInt + 1).toString

      Ok( welcome ).map(x => {
        x.addCookie(
            ResponseCookie(
              name + "_min",
              min,
              httpOnly = true,
              secure = true,
              sameSite = SameSite.Strict,
              path = Some("/user/" + name)
            )
          )
          .addCookie(
            ResponseCookie(
              name + "_max",
              max,
              httpOnly = true,
              secure = true,
              sameSite = SameSite.Strict,
              path = Some("/user/" + name)
            )
          )
          .addCookie(
            ResponseCookie(
              name + "_guesses",
              guesses,
              httpOnly = true,
              secure = true,
              sameSite = SameSite.Strict,
              path = Some("/user/" + name)
            )
          )
          .addCookie(
            ResponseCookie(
              name + "_server",
              serverGuess,
              httpOnly = true,
              secure = true,
              sameSite = SameSite.Strict,
              path = Some("/user/" + name)
            )
          )
      })

    /*
     Check a guess from a client, return answer.

     http://127.0.0.1:9001/user/maris/guess/3
     */
    case req @ GET -> Root / "user" / name / "guess" / guess =>
      val victory  = "You won. Congratulations!"
      val failure  = s"Hello, $name! You lost. Sorry!"
      val tryAgain = s"Hello, ${name.capitalize}! Your guess is: $guess"

      val counterCookie: Option[RequestCookie] = req.cookies.find(_.name == name + "_guesses")
      val counterValue: Int                    = counterCookie.flatMap(_.content.toIntOption).fold(1)(_ - 1)

      val resultCookie: Option[RequestCookie] =
        req.cookies.find(_.name == name + "_server")
      val resultValue: Int = resultCookie.flatMap(_.content.toIntOption).get

      if (guess.toInt == resultValue) Ok(victory)
      else if (counterValue == 0) Ok(failure)
      else {
        Ok(tryAgain).map(x => {
          x.addCookie(
            ResponseCookie(
              name + "_guesses",
              counterValue.toString,
              httpOnly = true,
              secure = true,
              sameSite = SameSite.Strict,
              path = Some("/user/" + name)
            )
          )
        })
      }
  }

  private[http] val httpApp = { gameRoutes }.orNotFound

  override def run(args: List[String]): IO[ExitCode] =
    BlazeServerBuilder[IO](ExecutionContext.global)
      .bindHttp(port = 9001, host = "localhost")
      .withHttpApp(httpApp)
      .serve
      .compile
      .drain
      .as(ExitCode.Success)
}

object GuessClient extends IOApp {
  import org.http4s.client._

  private val uri = uri"http://127.0.0.1:9001"

  // Generate some random values for client.
  val rand: Random.type = scala.util.Random
  val min: Int          = rand.between(0, 100)
  val max: Int          = rand.between(min + 1, 200)
  val guesses: Int      = (max - min) / rand.nextInt(10) + 1
  val name: String      = Random.alphanumeric.take(10).mkString

  private def printLine(string: String = ""): IO[Unit] = IO(println(string))

  def generateMoves(client: Client[IO]): IO[Unit] = {

    @tailrec
    def loop(guessesLeft: Int): IO[Unit] = {
      val currentGuess = rand
        .between(min, max + 1)
        .toString

      println("Guessing: " + currentGuess)

      val results: IO[String] = client
        .expect[String](
          uri / "user" / name / "guess" / currentGuess
        )

      (for {
        x <- client.expect[String](uri / "user" / name / "guess" / currentGuess)
      } yield println(x)).unsafeRunAsyncAndForget()

      if (guessesLeft == 0) IO()
      else loop(guessesLeft - 1)
    }

    loop(guesses - 1)
  }

  def run(args: List[String]): IO[ExitCode] = {
    BlazeClientBuilder[IO](ExecutionContext.global).resource
      .use { client =>
        for {
          _ <- printLine(string = "Start Server:")
          _ <- client.expect[String](
            uri / "user" / name / "min" / min.toString / "max" / max.toString /
              "guesses" / guesses.toString
          ) >>= printLine
          _ <- printLine()
          _ <- printLine(string = "Start Guessing:")
          _ <- generateMoves(client)
          _ <- printLine()
        } yield ()
      }
      .as(ExitCode.Success)
  }
}
