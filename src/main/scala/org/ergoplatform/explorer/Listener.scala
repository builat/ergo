package org.ergoplatform.explorer

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import cats.effect.IO
import doobie.hikari.HikariTransactor
import org.ergoplatform.mining.emission.CoinsEmission
import org.ergoplatform.modifiers.ErgoFullBlock
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.{SemanticallySuccessfulModifier, SyntacticallySuccessfulModifier}
import scorex.core.utils.ScorexLogging

import scala.io.Source


class Listener(host: String, user: String, pass: String, ce: CoinsEmission) extends Actor with ScorexLogging {

  import doobie._ ,doobie.implicits._
  import cats.data._ ,cats.implicits._

  implicit val ec = context.system.dispatcher

  val xa: HikariTransactor[IO] = (for {
    xa <- HikariTransactor.newHikariTransactor[IO](
      driverClassName = "org.postgresql.Driver",
      url = host,
      user = user,
      pass = pass
    )
    _ <- xa.configure(_ => IO(()))
  } yield xa).unsafeRunSync()

  override def preStart(): Unit = {
    logger.info("Starting blockchain listener for explorer.")
    context.system.eventStream.subscribe(self, classOf[SemanticallySuccessfulModifier[_]])
    context.system.eventStream.subscribe(self, classOf[SyntacticallySuccessfulModifier[_]])
  }

  override def receive: Receive = {
    case SemanticallySuccessfulModifier(mod: ErgoFullBlock) =>

      val r = proceedBlock(mod)
      val modSize = mod.header.bytes.length + mod.blockTransactions.bytes.length + mod.aDProofs.map(_.bytes.length).getOrElse(0)
      val lastStats = readLastStats
      val txsCount = mod.blockTransactions.transactions.length.toLong
      val coins = mod.blockTransactions.transactions.head.outputCandidates.map(_.value).sum
      val miningTime = if (lastStats.timestamp == 0L) { 0L } else {mod.header.timestamp - lastStats.timestamp}
      val coinsTotal = ce.coinsTotal - ce.remainingCoinsAfterHeight(mod.header.height.toLong)

      val updatedStats = StatRecord(
        timestamp = mod.header.timestamp,
        blockSize = modSize.toLong,
        totalSize = lastStats.totalSize + modSize,
        transactionCount = txsCount,
        totalTransactionsCount = lastStats.totalTransactionsCount + txsCount,
        blocksCount = lastStats.blocksCount + 1L,
        difficulty = mod.header.requiredDifficulty.toLong,
        blockCoins = coins,
        totalCoins = lastStats.totalCoins + coins,
        blockValue = mod.blockTransactions.transactions.tail.flatMap(_.outputCandidates).map(_.value).sum,
        totalMiningTime = lastStats.totalMiningTime + miningTime,
        blockMiningTime = miningTime,
        version = org.ergoplatform.Version.VersionString,
        height = mod.header.height,
        totalCoinsIssued = coinsTotal
      )
      writeStats(updatedStats)
      logger.info(s"Block has been inserted, result $r.")
      logger.info(s"Stats have been written.")
  }

  def proceedBlock(mod: ErgoFullBlock): Int = DBService.proceedBlock(mod, ce, xa).unsafeRunSync()

  def readLastStats: StatRecord = DBService.readLastStats(xa).unsafeRunSync().getOrElse(StatRecord())
  def writeStats(s: StatRecord): Int = DBService.writeStats(s, xa).unsafeRunSync()
}

object ListenerRef {

  def props(host: String, user: String, pass: String, ce: CoinsEmission): Props = Props.create(classOf[Listener], host, user, pass, ce)

  def apply(host: String, user: String, pass: String, ce: CoinsEmission)
           (implicit system: ActorSystem): ActorRef = system.actorOf(props(host, user, pass, ce))

}

case class MarketInfo(supply: Long, marketCap: Long, marketPriceUsd: Long)
