package org.ergoplatform.explorer

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import cats.effect.IO
import doobie.hikari.HikariTransactor
import org.ergoplatform.mining.emission.CoinsEmission
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.BlockTransactions
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.{SemanticallySuccessfulModifier, SyntacticallySuccessfulModifier}
import scorex.core.utils.ScorexLogging
import scorex.crypto.encode.Base16

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
      val (reward, fee) = minerRewardAndFee(mod.blockTransactions, mod.header.height, ce)


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
        totalCoinsIssued = coinsTotal,
        minerRevenue = reward + fee
      )
      writeStats(updatedStats)
      logger.info(s"Block has been inserted, result $r.")
      logger.info(s"Stats have been written.")
  }

  def proceedBlock(mod: ErgoFullBlock): Int = DBService.proceedBlock(mod, ce, xa).unsafeRunSync()

  def readLastStats: StatRecord = DBService.readLastStats(xa).unsafeRunSync().getOrElse(StatRecord())
  def writeStats(s: StatRecord): Int = DBService.writeStats(s, xa).unsafeRunSync()


  private def minerRewardAndFee(bt: BlockTransactions, height: Int, ce: CoinsEmission): (Long, Long) = {
    val bite = "968400020191a3c6a70300059784000" +
      "201968400030193c2a7c2b2a505000000000000000093958fa30500" +
      "000000000027600500000001bf08eb00990500000001bf08eb009c05" +
      "0000000011e1a3009a0500000000000000019d99a3050000000000002" +
      "76005000000000000087099c1a7c1b2a505000000000000000093c6b2a" +
      "5050000000000000000030005a390c1a7050000000011e1a300"

    val reward = ce.emissionAtHeight(height.toLong)
    val tx = bt.transactions.find(t => t.outputCandidates.headOption.map(v => Base16.encode(v.propositionBytes)).contains(bite))
    val fee = tx.fold(0L) { t =>
      t.outputCandidates.drop(1).headOption.map { v => v.value - reward }.getOrElse(0L)
    }
    (reward, fee)
  }
}

object ListenerRef {

  def props(host: String, user: String, pass: String, ce: CoinsEmission): Props = Props.create(classOf[Listener], host, user, pass, ce)

  def apply(host: String, user: String, pass: String, ce: CoinsEmission)
           (implicit system: ActorSystem): ActorRef = system.actorOf(props(host, user, pass, ce))

}

case class MarketInfo(supply: Long, marketCap: Long, marketPriceUsd: Long)
