package org.ergoplatform.explorer

object StatsWriter {

  val table = "blockchain_stats"
  val fields: Seq[String] = Seq(
    "ts",
    "block_size",
    "total_size",
    "txs_count",
    "txs_total_count",
    "blocks_count",
    "difficulty",
    "block_coins",
    "total_coins",
    "block_value",
    "block_fee",
    "total_mining_time",
    "block_mining_time",
    "supply",
    "market_cap",
    "hashrate",
    "version"
  )

  val fieldsString = fields.mkString(", ")

  def statToInsertString(s: StatRecord): String = s
    .productIterator
    .toList
    .map{ v => if (v.isInstanceOf[String]) { s"'$v'"} else v }
    .map(_.toString)
    .mkString("(", ",", ")")
}
