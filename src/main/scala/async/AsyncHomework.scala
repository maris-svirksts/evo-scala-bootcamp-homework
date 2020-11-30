package async

import java.net.URL
import java.util.concurrent.Executors

import scala.concurrent.{ExecutionContext, Future}
import scala.io.Source
import scala.util.{Failure, Success}

/**
  * Application:
  * - takes a web-page URL from arguments (args array)
  * - loads the web-page body, extracts HTTP links from it
  * - for all the found links, tries to fetch a server name header if there is one
  * - prints all the encountered unique server name values in alphabetical order
  *
  * Each link processing should be done in parallel.
  * Validation of arguments is not needed.
  *
  * Try to test it on http://google.com!
  */
object AsyncHomework extends App {
  private implicit val ec: ExecutionContext =
    ExecutionContext.fromExecutor(Executors.newCachedThreadPool())

  // https://gist.github.com/jsrikrishna/7db4b47514f7cf2ce171 : Scala Future.sequence example
  // https://alvinalexander.com/scala/scala-shell-script-command-line-arguments-args/

  private val urlToCheck: String =
    if (args.length == 0) "https://www.google.com/"
    else args(0)

  fetchPageBody(urlToCheck).onComplete {
    case Success(body) =>
      findLinkUrls(body).onComplete {
        case Success(links) =>
          val getNames = links.map(x => fetchServerName(x))
          val gathered = Future.sequence(getNames)

          // Changed to set to remove duplicates, then back to list for ordering. IntelliJ suggested exchanging it with .distinct
          gathered.onComplete {
            case Success(serverNames) =>
              val results =
                serverNames
                  .map(x => x.getOrElse("Unknown").toUpperCase)
                  .distinct
                  .sorted

              // Return results.
              println()
              println("List of server names:")
              println(results)
            case Failure(t) =>
              println("fetchServerName failure.")
              t.printStackTrace()
          }

        case Failure(t) =>
          println("findLinkUrls failure.")
          t.printStackTrace()
      }
    case Failure(t) =>
      println("fetchPageBody failure.")
      t.printStackTrace()
  }

  private def fetchPageBody(url: String): Future[String] = {
    println(f"Fetching $url")
    Future {
      val source = Source.fromURL(url)
      try {
        source.mkString
      } finally {
        source.close()
      }
    }
  }

  private def fetchServerName(url: String): Future[Option[String]] = {
    println(s"Fetching server name header for $url")
    Future {
      Option(new URL(url).openConnection().getHeaderField("Server"))
    }
  }

  private def findLinkUrls(html: String): Future[List[String]] =
    Future {
      val linkPattern = """href="(http[^"]+)"""".r
      linkPattern.findAllMatchIn(html).map(m => m.group(1)).toList
    }
}
