package org.ergoplatform.explorer

import org.ergoplatform.modifiers.history.{ADProofs, BlockTransactions, Header}
import scorex.crypto.encode.Base16

object HeaderWriter {


  val table = "headers"
  val fields: Seq[String] = Seq(
    "id",
    "parent_id",
    "version",
    "height",
    "ad_proofs_root",
    "state_root",
    "transactions_root",
    "ts",
    "n_bits",
    "extension_hash",
    "block_size",
    "equihash_solution",
    "ad_proofs",
    "tx_count",
    "miner_name",
    "miner_address"
  )

  val fieldsString = fields.mkString("(", ", ", ")")

  def minerAddress(bt: BlockTransactions): String = {

    val bite = "968400020191a3c6a70300059784000" +
      "201968400030193c2a7c2b2a505000000000000000093958fa30500" +
      "000000000027600500000001bf08eb00990500000001bf08eb009c05" +
      "0000000011e1a3009a0500000000000000019d99a3050000000000002" +
      "76005000000000000087099c1a7c1b2a505000000000000000093c6b2a" +
      "5050000000000000000030005a390c1a7050000000011e1a300"

    val tx = bt.transactions.find(t => t.outputCandidates.headOption.map(v => Base16.encode(v.propositionBytes)).contains(bite))
    tx.fold("unknown_miner") { t =>
      t.outputCandidates.drop(1).headOption.map{v => Base16.encode(v.propositionBytes)}.getOrElse("unknown_miner")
    }

  }

  def dataString(h: Header, adProofs: Option[ADProofs], bt: BlockTransactions): String = {
    val id = Base16.encode(h.id)
    val pId = Base16.encode(h.parentId)
    val apr = Base16.encode(h.ADProofsRoot)
    val sr = Base16.encode(h.stateRoot)
    val tr = Base16.encode(h.transactionsRoot)
    val size = h.serializer.toBytes(h).length
    val es = h.equihashSolution.ints.mkString("{", ", ", "}")
    val adp= adProofs.fold("{}") { p => p.bytes.mkString("{", ", ", "}") }
    val txCount = bt.transactions.length
    val extHash = Base16.encode(h.extensionHash)
    val miner = minerAddress(bt)

    s"('$id', '$pId', ${h.version}, ${h.height}, '$apr', '$sr', '$tr', " +
      s"${h.timestamp}, ${h.nBits}, '$extHash', $size, '$es','$adp', $txCount, '$miner', '$miner')"
  }

  def dataStrings(list: List[(Header, Option[ADProofs], BlockTransactions)]): String = list
    .map{ case (h, p, bt) => dataString(h, p, bt) }
    .mkString(", ")

  def dataStringsWithoutAdProofs(list: List[(Header, BlockTransactions)]): String = list
    .map{ case (h, bt) => dataString(h, None, bt) }
    .mkString(", ")
}
