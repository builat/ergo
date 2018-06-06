package org.ergoplatform.explorer

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import cats.effect.IO
import doobie.hikari.HikariTransactor
import org.ergoplatform.modifiers.ErgoFullBlock
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.{SemanticallySuccessfulModifier, SyntacticallySuccessfulModifier}
import scorex.core.utils.ScorexLogging

import scala.io.Source


class Listener(host: String, user: String, pass: String) extends Actor with ScorexLogging {

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

      val marketInfo = getMarketInfo()
      val r = proceedBlock(mod)
      val modSize = mod.header.bytes.length + mod.blockTransactions.bytes.length + mod.aDProofs.map(_.bytes.length).getOrElse(0)
      val lastStats = readLastStats
      val txsCount = mod.blockTransactions.transactions.length.toLong
      val coins = mod.blockTransactions.transactions.head.outputCandidates.map(_.value).sum
      val miningTime = if (lastStats.timestamp == 0L) { 0L } else {mod.header.timestamp - lastStats.timestamp}

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
        supply = marketInfo.supply,
        marketCap = marketInfo.marketCap,
        hashRate = 0L,
        marketPriceUsd = marketInfo.marketPriceUsd
      )
      writeStats(updatedStats)
      logger.info(s"Block has been inserted, result $r.")
      logger.info(s"Stats have been written.")
  }

  def proceedBlock(mod: ErgoFullBlock): Int = DBService.proceedBlock(mod, xa).unsafeRunSync()

  def readLastStats: StatRecord = DBService.readLastStats(xa).unsafeRunSync().getOrElse(StatRecord())
  def writeStats(s: StatRecord): Int = DBService.writeStats(s, xa).unsafeRunSync()


  def getMarketInfo(): MarketInfo = {
    import io.circe._
    import io.circe.parser._

    val string = Source.fromURL("https://api.coinmarketcap.com/v2/ticker/1762/?convert=BTC", "UTF8").mkString
    val json = parse(string).getOrElse(Json.Null)
    val cur = json.hcursor

    val usPrice = cur
      .downField("data").downField("quotes").downField("USD").downField("price").as[Double].getOrElse(0.0)
    val usMarketCap = cur
      .downField("data").downField("quotes").downField("USD").downField("market_cap").as[Long].getOrElse(0L)
    val totalSupply = cur
      .downField("data").downField("total_supply").as[Double].getOrElse(0.0)
    MarketInfo(totalSupply.toLong, usMarketCap, (usPrice * 100).toLong)
  }
}

object ListenerRef {

  def props(host: String, user: String, pass: String): Props = Props.create(classOf[Listener], host, user, pass)

  def apply(host: String, user: String, pass: String)
           (implicit system: ActorSystem): ActorRef = system.actorOf(props(host, user, pass))

}

case class MarketInfo(supply: Long, marketCap: Long, marketPriceUsd: Long)
