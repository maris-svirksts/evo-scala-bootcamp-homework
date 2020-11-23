package effects

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

/*
 * Homework 1. Provide your own implementation of a subset of `IO` functionality.
 *
 * Provide also tests for this functionality in EffectsHomework1Spec (which you should create).
 *
 * Refer to:
 *  - https://typelevel.org/cats-effect/datatypes/io.html
 *  - https://typelevel.org/cats-effect/api/cats/effect/IO$.html
 *  - https://typelevel.org/cats-effect/api/cats/effect/IO.html
 * about the meaning of each method as needed.
 *
 * There are two main ways how to implement IO:
 * - Executable encoding  - express every constructor and operator for our model in terms of its execution
 * - Declarative encoding - express every constructor and operator for our model as pure data in a recursive
 *                          tree structure
 *
 * While the real Cats Effect IO implementation uses declarative encoding, it will be easier to solve this
 * task using executable encoding, that is:
 *  - Add a `private val run: () => A` parameter to the class `IO` private constructor
 *  - Have most of the methods return a `new IO(...)`
 *
 * Ask questions in the bootcamp chat if stuck on this task.
 */
object EffectsHomework1Executable {
  final class IO[A] private (val run: () => A) {
    def map[B](f: A => B): IO[B] = IO(f(unsafeRunSync()))
    def flatMap[B](f: A => IO[B]): IO[B] = this.map(f).unsafeRunSync()
    def *>[B](another: IO[B]): IO[B] = this.flatMap(x => another)
    def as[B](newValue: => B): IO[B] = this.map(_ => newValue)
    def void: IO[Unit] = this as ()
    def attempt: IO[Either[Throwable, A]] = IO(Try(unsafeRunSync()).toEither)
    def option: IO[Option[A]] = IO(Try(unsafeRunSync()) match {
      case Failure(e) => None
      case Success(v) => Some(v)
    })
    def handleErrorWith[AA >: A](f: Throwable => IO[AA]): IO[AA] = IO(Try(unsafeRunSync()) match {
      case Failure(e) => f(e).unsafeRunSync()
      case Success(v) => v
    })
    def redeem[B](recover: Throwable => B, map: A => B): IO[B] = IO(Try(unsafeRunSync()) match {
      case Failure(e) => recover(e)
      case Success(v) => map(v)
    })
    def redeemWith[B](recover: Throwable => IO[B], bind: A => IO[B]): IO[B] = attempt.unsafeRunSync() match {
      case Left(v) => recover(v)
      case Right(v) => bind(v)
    }
    def unsafeRunSync(): A = run()
    def unsafeToFuture(): Future[A] = Future(unsafeRunSync())
  }

  object IO {
    def apply[A](body: => A): IO[A] = delay(body)
    def suspend[A](thunk: => IO[A]): IO[A] = apply(thunk.run())
    def delay[A](body: => A): IO[A] = new IO[A](() => body)
    def pure[A](a: A): IO[A] = new IO(() => a)
    def fromEither[A](e: Either[Throwable, A]): IO[A] = IO(e match {
      case Left(e) => throw e
      case Right(v) => v
    })

    def fromOption[A](option: Option[A])(orElse: => Throwable): IO[A] =
      IO(option.getOrElse(throw orElse))

    def fromTry[A](t: Try[A]): IO[A] =
      IO(t match {
        case Success(v) => v
        case Failure(e) => throw e
      })

    def none[A]: IO[Option[A]] = pure(None)
    def raiseError[A](e: Throwable): IO[A] = throw e
    def raiseUnless(cond: Boolean)(e: => Throwable): IO[Unit] =
      IO(if (!cond) throw e)
    def raiseWhen(cond: Boolean)(e: => Throwable): IO[Unit] =
      IO(if (cond) throw e)
    def unlessA(cond: Boolean)(action: => IO[Unit]): IO[Unit] =
      IO(if (!cond) action)
    def whenA(cond: Boolean)(action: => IO[Unit]): IO[Unit] =
      IO(if (cond) action)
    val unit: IO[Unit] = IO(())
  }
}
