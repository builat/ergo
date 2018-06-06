package org.ergoplatform.explorer

import org.ergoplatform.modifiers.history.{BlockTransactions, Header}
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import scorex.crypto.encode.Base16

object TransactionsWriter {

  val table = "transactions"
  val fields: Seq[String] = Seq("id", "block_id", "is_coinbase", "ts")

  val fieldsString = fields.mkString("(", ", ", ")")

  def dataString(h: Header, tx: ErgoTransaction, isCoinbase: Boolean): String = {
    val id = Base16.encode(tx.id)
    val blockId = Base16.encode(h.id)
    val cb = if (isCoinbase) { "TRUE" } else { "FALSE" }
    s"('$id', '$blockId', $cb, ${h.timestamp})"
  }

  def dataStrings(h: Header, bt: BlockTransactions): String = {
    val coinbaseTx = dataString(h, bt.transactions.head, true)
    val other = bt.transactions.tail.map { t => dataString(h, t, false)}.toList
    (coinbaseTx :: other).mkString(", ")
  }

}
