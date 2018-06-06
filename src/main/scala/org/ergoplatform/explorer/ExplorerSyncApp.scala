package org.ergoplatform.explorer

import akka.actor.{ActorRef, ActorSystem, PoisonPill}
import org.ergoplatform.ErgoApp
import org.ergoplatform.settings.ErgoSettings
import scorex.core.utils.ScorexLogging

import scala.concurrent.Await
import scala.io.Source
import scala.util.Try

class ExplorerSyncApp(args: Seq[String]) extends ErgoApp(args) {

  override lazy val ergoSettings: ErgoSettings = ErgoSettings.read(None)

  val jdbcUrl = sys.env.get("DB_URL").getOrElse("jdbc:postgresql://localhost:5432/explorer")
  val dbUser = sys.env.get("DB_USER").getOrElse("ergo")
  val dbPass = sys.env.get("DB_PASS")
  val dbPassFile = sys.env.get("DB_PASS_FILE")

  def pass: String = (dbPass orElse readfile(dbPassFile)).getOrElse("pass")

  def readfile(filename: Option[String]): Option[String] = filename.flatMap { f =>
    Try(Source.fromFile(f, "UTF8").mkString).toOption
  }

  logger.info("Starting app with db config:")
  logger.info(s"db url : $jdbcUrl")
  logger.info(s"db user : $dbUser")
  logger.info(s"db pass : $pass")

  val listener = ListenerRef(jdbcUrl, dbUser, pass)

  override val actorsToStop =
    Seq(
      minerRef,
      peerManagerRef,
      networkControllerRef,
      readersHolderRef,
      nodeViewSynchronizer,
      statsCollectorRef,
      nodeViewHolderRef,
      listener)
}

object ExplorerSyncApp extends ScorexLogging {

  import scala.concurrent.duration._

  def main(args: Array[String]): Unit = new ExplorerSyncApp(args).run()

  def forceStopApplication(code: Int = 1): Nothing = sys.exit(code)

  def shutdown(system: ActorSystem, actors: Seq[ActorRef]): Unit = {
    log.warn("Terminating Actors")
    actors.foreach { a => a ! PoisonPill }
    log.warn("Terminating ActorSystem")
    val termination = system.terminate()
    Await.result(termination, 60.seconds)
    log.warn("Application has been terminated.")
  }

}
