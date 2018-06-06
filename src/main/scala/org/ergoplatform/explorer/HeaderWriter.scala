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
    val miner = Base16.encode(bt.transactions.head.outputCandidates.head.propositionBytes)

    s"('$id', '$pId', ${h.version}, ${h.height}, '$apr', '$sr', '$tr', " +
      s"${h.timestamp}, ${h.nBits}, '', $size, '$es','$adp', $txCount, '$miner', '$miner')"
  }

  def dataStrings(list: List[(Header, Option[ADProofs], BlockTransactions)]): String = list
    .map{ case (h, p, bt) => dataString(h, p, bt) }
    .mkString(", ")

  def dataStringsWithoutAdProofs(list: List[(Header, BlockTransactions)]): String = list
    .map{ case (h, bt) => dataString(h, None, bt) }
    .mkString(", ")
}
